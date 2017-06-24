signature ORD = sig
  type t
  val cmp : t * t -> order
end

signature EQ = sig
  type t
  val eq : t * t -> bool
end

signature HASHABLE = sig
  structure E : EQ
  val hash : E.t -> int
end
