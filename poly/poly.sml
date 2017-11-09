signature POLYABLE = sig
  type 'a t
  type 'a succ
end

signature T = sig
  type 'a t
end

signature POLY = sig
  type 'a t
  type 'a succ

  val fix : (('a succ t -> 'b) -> 'a t -> 'b) -> 'a t -> 'b

  functor Ret (P : T) : sig
    val fix :
      (('a succ t -> 'b succ P.t) -> 'a t -> 'b P.t) -> 'a t -> 'b P.t
  end

  functor Curry (P : T) : sig
    val fix :
      (('a succ P.t -> 'a succ t -> 'b) -> 'a P.t -> 'a t -> 'b) -> 'a P.t -> 'a t -> 'b
  end
end

functor Poly (P : POLYABLE) : POLY
  where type 'a t = 'a P.t
  and type 'a succ = 'a P.succ
= struct
  open P
  fun fix f y = f (Unsafe.cast (fix f)) y
  functor Ret (P : T) = struct val fix = fix end
  functor Curry (P : T) = struct val fix = fix end
end

structure Examples = struct
  datatype 'a btree = E | N of 'a * ('a * 'a) btree

  structure P = Poly (struct
    type 'a t = 'a btree
    type 'a succ = 'a * 'a
  end)

  val count = fn count =>
    fn E => 0
     | N (_, xs) => 1 + 2 * count xs

  val count = fn T => P.fix count T

  fun unpackPairs xs =
    List.foldl (fn ((x, y), acc) => x :: y :: acc) [] xs

  val toList = fn toList =>
    fn E => []
     | N (x, xs) => x :: unpackPairs (toList xs)

  structure PL = P.Ret (struct
    type 'a t = 'a list
  end)

  val toList = fn T => PL.fix toList T

  val sum = fn sum => fn f =>
    fn E => 0
     | N (x, xs) => f x + sum (fn (a, b) => f a + f b) xs

  structure PF = P.Curry (struct
    type 'a t = 'a -> int
  end)

  val sum = fn T => PF.fix sum (~o~) T
end
