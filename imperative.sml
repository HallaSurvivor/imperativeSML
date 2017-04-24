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
              | OutputOne
              | OutputTwo
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
val parseOutOne   = gen #"." OutputOne
val parseOutTwo   = gen #":" OutputTwo
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
               <|> parseOutOne
               <|> parseOutTwo
               <|> parseXorOne
               <|> parseXorTwo
               <|> parseLoop
in
  (many parseInsn) stream
end


(* interpreting *)

type tape = IntInf.int list * IntInf.int * IntInf.int list
type tapes = tape * tape

type runner = (unit, tapes) state

fun runInsn MoveOneRight = modify
  (fn (([],   x,rs),ts) => (([],0 : IntInf.int,x::rs),ts) 
   |  ((l::ls,x,rs),ts) => ((ls,l,x::rs),ts)
  )

  | runInsn MoveTwoRight = modify
  (fn (os,([],   x,rs)) => (os,([],0 : IntInf.int,x::rs)) 
   |  (os,(l::ls,x,rs)) => (os,(ls,l,x::rs))
  )

  | runInsn MoveOneLeft = modify
  (fn ((ls,x,[]   ),ts) => ((x::ls,0 : IntInf.int,[]),ts) 
   |  ((ls,x,r::rs),ts) => ((x::ls,r,rs),ts)
  )

  | runInsn MoveTwoLeft = modify
  (fn (os,(ls,x,[]   )) => (os,(x::ls,0 : IntInf.int,[])) 
   |  (os,(ls,x,r::rs)) => (os,(x::ls,r,rs))
  )

  | runInsn IncrementOne = modify
  (fn ((ls,x,rs),ts) => ((ls,x+1,rs),ts))

  | runInsn IncrementTwo = modify
  (fn (os,(ls,x,rs)) => (os,(ls,x+1,rs)))

  | runInsn DecrementOne = modify
  (fn ((ls,x,rs),ts) => ((ls,x-1,rs),ts))

  | runInsn DecrementTwo = modify
  (fn (os,(ls,x,rs)) => (os,(ls,x-1,rs)))

  | runInsn OutputOne = 
  get >>= (fn ((_,x,_),_) => (return o print o Char.toString o Char.chr o IntInf.toInt) x)

  | runInsn OutputTwo = 
  get >>= (fn (_,(_,x,_)) => (return o print o Char.toString o Char.chr o IntInf.toInt) x)

  | runInsn XorOne = modify
  (fn ((ls,x,rs),(ls',y,rs')) => ((ls,IntInf.xorb (x,y),rs),(ls',y,rs')))

  | runInsn XorTwo = modify
  (fn ((ls,x,rs),(ls',y,rs')) => ((ls,x,rs),(ls',IntInf.xorb (x,y),rs')))

  | runInsn (lp as Loop is) = 
  get >>= (fn ((_,x,_), (_,y,_)) => if x = y then return () else runInsns is >> runInsn lp)

and runInsns xs = mapM_ runInsn xs

fun show ((ls,x,rs),(ls',x',rs')) = 
let
  val t1 = "Tape 1: ["^
    (String.concatWith ", " (map (Int.toString o IntInf.toInt) (ls@(x::rs))))^"]"

  val t2 = "Tape 2: ["^
    (String.concatWith ", " (map (Int.toString o IntInf.toInt) (ls'@(x'::rs'))))^"]"
in
  t1^"\n"^t2
end

fun main s = 
  case (parseInsns o String.explode) s of
    (LEFT e, _)   => (print e; 1)
  | (RIGHT is, _) => 
      let
        val initialState = (([],0 : IntInf.int,[]),([],0 : IntInf.int,[]))
        val st = runInsns is
        val endState = evalState st initialState
      in
        0
      end
