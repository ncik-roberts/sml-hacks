signature ORD = sig
  type t
  val cmp : t * t -> order
end

signature EQ = sig
  type t
  val eq : t * t -> bool
end

signature HASHABLE = sig
  type t
  val eq : t * t -> bool
  val hash : t -> int
  val toString : t -> string
end

structure IntElem : HASHABLE = struct
  type t = int
  val eq = (op =) : t * t -> bool
  val toString = Int.toString
  val hash = ~ o ~
end
