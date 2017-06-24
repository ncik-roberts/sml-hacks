signature CACHE = sig
  type key
  type 'a cache

  (* Optional size hint to create cache *)
  val init : int -> 'a cache

  (* Look up key in cache, calculating using function if not found *)
  val lookup : 'a cache -> (key -> 'a) -> key -> 'a
end

(* Statically sized hashtable with chaining *)
functor HashTable (H : HASHABLE) :> CACHE
  where type key = H.E.t
= struct
  type key = H.E.t
  type 'a cache = (key * 'a) list Array.array

  fun init n = Array.array (n, [])

  fun lookup cache f key =
    let
      val i = H.hash key
      val candidates = Array.sub (cache, i)
    in
      case List.find (fn (k, _) => H.E.eq (k, key)) candidates
        of SOME (_, v) => v
         | NONE =>
             let
               val v = f key
             in
               (Array.update (cache, i, (key, v) :: candidates);
                v)
             end
    end
end
