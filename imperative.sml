(* Christopher Grossack - 2017    *)
(* Two tape turing machine in SML *)
(* Heavily inspired by brainfuck  *)
(* (unobfuscated)                 *)

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

fun ch c = satisfy (fn x => x = c)

fun gen x y = ch x *> y

val parseOneRight = gen ">" MoveOneRight
val parseTwoRight      = gen ")" MoveTwoRight
val parseOneLeft  = gen "<" MoveOneLeft
val parseTwoLeft  = gen "(" MoveTwoLeft
val parseIncOne   = gen "+" IncrementOne
val parseIncTwo   = gen "*" IncrementTwo
val parseDecOne   = gen "-" DecrementOne
val parseDecTwo   = gen "/" DecrementTwo
val parseOutOne = gen "." OutputOne
val parseOutTwo = gen ":" OutputTwo
val parseInOne = gen "," InputOne
val parseInTwo = gen ";" InputTwo
val parseNotOne = gen "~" NotOne
val parseNotTwo = gen "`" NotTwo
val parseXorOne = gen "^" XorOne
val parseXorTwo = gen "%" XorTwo
val parseAndOne = gen "&" AndOne
val parseAndTwo = gen "@" AndTwo
val parseOrOne = gen "|" OrOne
val parseOrTwo = gen "!" OrTwo
val parseLoop = ch "[" *> Loop o parseInsns <* ch "]"
