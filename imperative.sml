(* Christopher Grossack - 2017    *)
(* Two tape turing machine in SML *)
(* Heavily inspired by brainfuck  *)
(* (unobfuscated)                 *)

use "parsec.sml";

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
              | InputOne
              | InputTwo
              | NotOne
              | NotTwo
              | XorOne
              | XorTwo
              | AndOne
              | AndTwo
              | OrOne
              | OrTwo
              | Loop of Insn list

fun fst (a,b) = a

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
val parseInOne    = gen #"," InputOne
val parseInTwo    = gen #";" InputTwo
val parseNotOne   = gen #"~" NotOne
val parseNotTwo   = gen #"`" NotTwo
val parseXorOne   = gen #"^" XorOne
val parseXorTwo   = gen #"%" XorTwo
val parseAndOne   = gen #"&" AndOne
val parseAndTwo   = gen #"@" AndTwo
val parseOrOne    = gen #"|" OrOne
val parseOrTwo    = gen #"!" OrTwo

fun parseLoop s = 
let 
  val parse' = Loop <$> parseInsns
in
  (ch #"[" *> parse' <* ch #"]") s
end
and parseInsns stream = 
let
  val parseInsn = parseOneRight 
               <|> parseTwoRight 
               <|> parseOneLeft
               <|> parseTwoLeft
               <|> parseIncOne
               <|> parseIncTwo
               <|> parseDecOne
               <|> parseDecTwo
               <|> parseOutOne
               <|> parseOutTwo
               <|> parseInOne
               <|> parseInTwo
               <|> parseNotOne
               <|> parseNotTwo
               <|> parseXorOne
               <|> parseXorTwo
               <|> parseAndOne
               <|> parseAndTwo
               <|> parseOrOne
               <|> parseOrTwo
               <|> parseLoop
in
  (many parseInsn) stream
end
