(* a simple parsec clone in sml *)
(* Christopher Grossack - 2017  *)

datatype 'a error  = LEFT of string | RIGHT of 'a
type ('a,'s) parser = 's -> ('a error * 's)

(* applicatives are cool *)
infix 0 <$>
fun f <$> p = 
  fn s => case p s of 
    (LEFT e, res) => (LEFT e, res) | (RIGHT c, res) => (RIGHT (f c), res)

fun pure c = fn s => (RIGHT c, s)

infix 0 <*>
fun ff <*> xx = fn s =>
  case ff s of 
    (LEFT e, s')  => (LEFT e, s')
  | (RIGHT f, s') => case xx s' of
                      (LEFT e, s'')  => (LEFT e, s'')
                    | (RIGHT x, s'') => (RIGHT (f x), s'')

infix 0 *>
fun u *> v = pure (fn _ => fn x => x) <*> u <*> v

infix 0 <*
fun u <* v = pure (fn x => fn _ => x) <*> u <*> v

(* back to parsing *)
fun satisfy f = fn s => 
  case s of
    []     => (LEFT "end of stream", [])
  | (c::cs) => if f c then (RIGHT c, cs) else (LEFT "satisfy failed", cs)

fun try p = fn s => 
  case p s of 
    (LEFT e, _)   => (LEFT e, s) 
  | (RIGHT c, s') => (RIGHT c, s')

infix <|>
fun p1 <|> p2 = fn s =>
  case p1 s of 
    (LEFT e, _) => p2 s
  | (RIGHT c, s') => (RIGHT c, s')

fun noneOf s = satisfy (fn c => String.isSubstring (String.str c) s)

fun many p = 
let
  fun p' s =
    case p s of
      (LEFT e, _)   => (RIGHT [], s)
    | (RIGHT c, s') => case p' s' of
                         (LEFT e, s'')   => (LEFT e, s'')
                       | (RIGHT cs, s'') => (RIGHT (c::cs), s'')
in
  p'
end
