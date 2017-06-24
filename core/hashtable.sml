signature CACHE = sig
  type key
  type 'a cache

  val init : unit -> 'a cache

  (* Look up key in cache, calculating using function if not found *)
  val lookup : 'a cache -> (key -> 'a) -> key -> 'a
end

(* Statically sized hashtable with chaining *)
functor HashTable (
  structure H : HASHABLE
  val size : int
) :> CACHE where type key = H.t
= struct
  type key = H.t
  type 'a cache = (key * 'a) list Array.array

  fun init () = Array.array (size, [])

  fun lookup cache f key =
    let
      val i = H.hash key mod size
      val candidates = Array.sub (cache, i)
    in
      case List.find (fn (k, _) => H.eq (k, key)) candidates
        of SOME (_, v) => v
         | NONE =>
             let
               val v = f key
             in
               (Array.update (cache, i, (key, v) :: candidates); v)
             end
    end
end
