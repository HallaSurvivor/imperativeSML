(* Christopher Grossack - 2017    *)
(* Two tape turing machine in SML *)
(* Heavily inspired by brainfuck  *)
(* (unobfuscated)                 *)

use "parsec.sml";
use "state.sml";

(* parsing *)
datatype Insn = MoveOneRight
              | MoveTwoRight
              | MoveOneLeft
              | MoveTwoLeft
              | IncrementOne
              | IncrementTwo
              | DecrementOne
              | DecrementTwo
              | XorOne
              | XorTwo
              | Loop of Insn list

fun ch c = satisfy (fn x => x = c)

fun gen x y = ch x *> pure y

val parseOneRight = gen #">" MoveOneRight
val parseTwoRight = gen #")" MoveTwoRight
val parseOneLeft  = gen #"<" MoveOneLeft
val parseTwoLeft  = gen #"(" MoveTwoLeft
val parseIncOne   = gen #"+" IncrementOne
val parseIncTwo   = gen #"*" IncrementTwo
val parseDecOne   = gen #"-" DecrementOne
val parseDecTwo   = gen #"/" DecrementTwo
val parseXorOne   = gen #"^" XorOne
val parseXorTwo   = gen #"v" XorTwo

fun parseLoop s = 
let 
  val parse' = Loop <$> parseInsns
in
  (ch #"[" *> parse' <* ch #"]") s
end

and parseInsns stream = 
let
  val parseInsn =  parseOneRight 
               <|> parseTwoRight 
               <|> parseOneLeft
               <|> parseTwoLeft
               <|> parseIncOne
               <|> parseIncTwo
               <|> parseDecOne
               <|> parseDecTwo
               <|> parseXorOne
               <|> parseXorTwo
               <|> parseLoop
in
  (many parseInsn) stream
end


(* interpreting *)

type tape = int list * int * int list
type tapes = tape * tape

type runner = (unit, tapes) state

fun runInsn MoveOneRight = modify
  (fn (([],   x,rs),ts) => (([],0,x::rs),ts) 
   |  ((l::ls,x,rs),ts) => ((ls,l,x::rs),ts)
  )

  | runInsn MoveTwoRight = modify
  (fn (os,([],   x,rs)) => (os,([],0,x::rs)) 
   |  (os,(l::ls,x,rs)) => (os,(ls,l,x::rs))
  )

  | runInsn MoveOneLeft = modify
  (fn ((ls,x,[]   ),ts) => ((x::ls,0,[]),ts) 
   |  ((ls,x,r::rs),ts) => ((x::ls,r,rs),ts)
  )

  | runInsn MoveTwoLeft = modify
  (fn (os,(ls,x,[]   )) => (os,(x::ls,0,[])) 
   |  (os,(ls,x,r::rs)) => (os,(x::ls,r,rs))
  )

  | runInsn IncOne = modify
  (fn ((ls,x,rs),ts) => ((ls,x+1,rs),ts))
  | runInsn IncTwo = modify
  (fn (os,(ls,x,rs)) => (os,(ls,x+1,rs)))

  | runInsn DecOne = modify
  (fn ((ls,x,rs),ts) => ((ls,x-1,rs),ts))

  | runInsn DecTwo = modify
  (fn (os,(ls,x,rs)) => (os,(ls,x-1,rs)))

  | runInsn XorOne = modify
  (fn ((ls,x,rs),(ls',y,rs')) => ((ls,IntInf.xorb (x,y),rs),(ls',y,rs')))

  | runInsn XorTwo = modify
  (fn ((ls,x,rs),(ls',y,rs')) => ((ls,x,rs),(ls',IntInf.xorb (x,y),rs')))

  | runInsn (lp as Loop is) = 
  get >>= (fn ((ls,x,rs), ts) => if x = 0 then return () else runInsns is >> runInsn lp)

and runInsns xs = mapM_ runInsn xs
