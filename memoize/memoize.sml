(*
 * Memoization framework for:
 *  - Recursive functions
 *  - A pair of mutually recursive functions
 *  - Arbitrarily many mutually recursive functions, feat. the Y combinator
 *)


(********************************************************************
 *                                MEMOIZE
 ********************************************************************
 *
 * Brings a non-recursive function to a memoized, combinated one
 *)
functor Memoize (C : CACHE) :> sig
  type t
  val memoize : ((t -> 'a) -> t -> 'a) -> t -> 'a
end where type t = C.key = struct
  type t = C.key

  fun memoize f =
    let
      val cache = C.init ()
      fun lookup key = C.lookup cache (f lookup) key
    in
      lookup
    end
end

structure MemoizeExample = struct
  structure M = Memoize (HashTable (
    structure H = IntElem
    val size = 10000
  ))
  val fib = M.memoize (fn f =>
       fn 0 => 1
        | 1 => 1
        | n => f (n-1) + f (n-2))
  fun seq n = List.tabulate (n, fib)
end


(*****************************************************************
 *                         MEMOIZE PAIR
 *****************************************************************
 *
 * Brings a pair of non-recursive ``mutually recursive'' functions to a pair
 * of memoized functions, where calling either of them memoizes for both.
 *)
functor MemoizePair (C : CACHE) :> sig
  type t
  type 'a f = t -> 'a
  val memoize : ('a f * 'b f -> 'a f) * ('a f * 'b f -> 'b f) -> 'a f * 'b f
end where type t = C.key = struct
  type t = C.key
  type 'a f = t -> 'a

  fun memoize (f, g) =
    let
      val (fCache, gCache) = (C.init (), C.init ())
      fun fLookup key = C.lookup fCache (f (fLookup, gLookup)) key
      and gLookup key = C.lookup gCache (g (fLookup, gLookup)) key
    in
      (fLookup, gLookup)
    end
end

structure MemoizePairExample = struct
  structure M = MemoizePair (HashTable (
    structure H = IntElem
    val size = 10000
  ))

  (*
   * Hofstadter male/female sequences, see this link:
   * http://mathworld.wolfram.com/HofstadterMale-FemaleSequences.html
   *)
  val female = fn (f, m) => fn 0 => 1
                             | n => n - m (f (n - 1))
  val male   = fn (f, m) => fn 0 => 0
                             | n => n - f (m (n - 1))
  val (female, male) = M.memoize (female, male)

  fun seq n = (List.tabulate (n, female), List.tabulate (n, male))
end

(***********************************************************
 *                   MEMOIZE LIST
 ***********************************************************
 *
 * Brings a list of arbitrarily many non-recursive ``mutually recursive''
 * functions to a list of memoized, combinatored functions
 *
 * Unfortunately, all of these functions must have the same type. To have
 * different types, you'll have to write a functor analogous to MemoizePair
 * for the correct arity.
 *
 * I really like this one because it doesn't use recursion at all (except
 * for map, which doesn't count in this case).
 *)
functor MemoizeMany (C : CACHE) :> sig
  type t
  type 'a f = t -> 'a
  val memoize : ('a f list -> 'a f) list -> 'a f list
end where type t = C.key = struct
  type t = C.key
  type 'a f = t -> 'a

  datatype 'a token = Token of ('a token -> 'a f) list

  fun lookup (f, cache) (t as Token fs) key =
    let
      val token = map (fn g => g t) fs
    in
      C.lookup cache (f token) key
    end

  fun memoize fs =
    let
      val cached = map (fn f => (f, C.init ())) fs
      val tokenized = map lookup cached
    in
      map (fn f => f (Token tokenized)) tokenized
    end
end

structure MemoizeManyExample = struct
  structure M = MemoizeMany (HashTable (
    structure H = IntElem
    val size = 30
  ))

  (*
   * "Draw" Sierpinski curve with a recurrence
   * https://en.wikipedia.org/wiki/Sierpi%C5%84ski_curve
   *
   * Add another comment.
   *)
  val sierpA = fn [ a, b, c, d ] =>
    fn 0 => []
     | level =>
        a (level-1) @ [ (#"A", 1, 1) ]
      @ b (level-1) @ [ (#"A", 2, 0) ]
      @ d (level-1) @ [ (#"A", 1, ~1) ]
      @ a (level-1)

  val sierpB = fn [ a, b, c, d ] =>
    fn 0 => []
     | level =>
        b (level-1) @ [ (#"B", ~1, 1) ]
      @ c (level-1) @ [ (#"B", 0, 2) ]
      @ a (level-1) @ [ (#"B", 1, 1) ]
      @ b (level-1)

  val sierpC = fn [ a, b, c, d ] =>
    fn 0 => []
     | level =>
        c (level-1) @ [ (#"C", ~1, ~1) ]
      @ d (level-1) @ [ (#"C", ~2, 0) ]
      @ b (level-1) @ [ (#"C", ~1, 1) ]
      @ c (level-1)

  val sierpD = fn [ a, b, c, d ] =>
    fn 0 => []
     | level =>
        d (level-1) @ [ (#"D", 1, ~1) ]
      @ a (level-1) @ [ (#"D", 0, ~2) ]
      @ c (level-1) @ [ (#"D", ~1, ~1) ]
      @ d (level-1)

  (* Replace with memoized versions *)
  val functions as [ sierpA, sierpB, sierpC, sierpD ]
    = M.memoize [ sierpA, sierpB, sierpC, sierpD ]

  (* Really only works nicely up to 12 :) *)
  fun seq n = map (fn f => List.tabulate (n, f)) functions
end
