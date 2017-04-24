(* a simple state monad clone in sml *)
(* Christopher Grossack - 2017       *)

type ('a,'s) state = 's -> ('a * 's)

fun return a = fn s => (a, s)

infix >>=
fun (st : ('a, 's) state) >>= (f : 'a -> ('b, 's) state) : ('b, 's) state = 
  fn s => let val (a, s') = st s in (f a) s' end

infix >>
fun st1 >> st2 = st1 >>= (fn _ => st2)

val get = fn s => (s,s)
fun put s  = fn _ => ((),s)

fun modify f = get >>= (put o f)

fun mapM_ f = foldr (fn (x,y) => f x >> y) (return ())
