(* packrat.sml

   Implementation of the PACKRAT signature.

   A `'a peg` is a function `int -> 'a result`: given a start position into the
   shared input string, it either succeeds (value + next position) or fails
   (furthest position reached). The shared input and a global generation
   counter live in mutable cells set by `parse`.

   Memoization. Each `rule` owns its own monomorphic memo, an array indexed by
   position holding `'a entry option`. SML has no existential types, so a single
   heterogeneous table across rules is not expressible; giving every rule its
   own array (of its own result type) is the portable workaround. A per-rule
   generation stamp lets us lazily invalidate the array between parses without
   reallocating, and the same machinery detects left recursion: a position whose
   entry is `InProgress` was re-entered before producing a result. *)

structure Packrat :> PACKRAT =
struct
  datatype 'a result = Ok of 'a * int | Err of int

  type 'a peg = int -> 'a result

  exception LeftRecursion of string

  (* shared parse state *)
  val theInput : string ref = ref ""
  val generation : int ref = ref 0
  val hits : int ref = ref 0

  fun inputSize () = String.size (!theInput)

  (* ---- terminals ---- *)

  fun chr c = fn pos =>
      if pos < inputSize () andalso String.sub (!theInput, pos) = c
      then Ok (c, pos + 1) else Err pos

  fun anyc pos =
      if pos < inputSize () then Ok (String.sub (!theInput, pos), pos + 1)
      else Err pos

  fun litc pred = fn pos =>
      if pos < inputSize () andalso pred (String.sub (!theInput, pos))
      then Ok (String.sub (!theInput, pos), pos + 1) else Err pos

  fun str s = fn pos =>
      let val n = String.size s in
        if pos + n <= inputSize ()
           andalso String.substring (!theInput, pos, n) = s
        then Ok (s, pos + n) else Err pos
      end

  (* ---- combinators ---- *)

  fun ret x = fn pos => Ok (x, pos)

  fun map f p = fn pos =>
      (case p pos of Ok (a, p') => Ok (f a, p') | Err e => Err e)

  fun seq p q = fn pos =>
      (case p pos of
           Ok (a, p') => (case q p' of Ok (b, p'') => Ok ((a, b), p'')
                                     | Err e => Err e)
         | Err e => Err e)

  fun seqL p q = map (fn (a, _) => a) (seq p q)
  fun seqR p q = map (fn (_, b) => b) (seq p q)

  fun alt p q = fn pos =>
      (case p pos of Ok r => Ok r | Err _ => q pos)

  fun opt p = fn pos =>
      (case p pos of Ok (a, p') => Ok (SOME a, p') | Err _ => Ok (NONE, pos))

  (* greedy many; iterative so long inputs don't overflow *)
  fun many p = fn pos =>
      let fun loop (acc, cur) =
              (case p cur of
                   Ok (a, next) =>
                     if next = cur then Ok (List.rev acc, cur)  (* no progress *)
                     else loop (a :: acc, next)
                 | Err _ => Ok (List.rev acc, cur))
      in loop ([], pos) end

  fun many1 p = fn pos =>
      (case (many p) pos of
           Ok ([], _) => Err pos
         | other => other)

  fun andP p = fn pos =>
      (case p pos of Ok _ => Ok ((), pos) | Err e => Err e)

  fun notP p = fn pos =>
      (case p pos of Ok _ => Err pos | Err _ => Ok ((), pos))

  fun delay thunk = fn pos => (thunk ()) pos

  (* ---- memoized rules ---- *)

  datatype 'a entry = Done of 'a result | InProgress

  fun rule name (thunk : unit -> 'a peg) : 'a peg =
      let
        (* lazily built once, on first use *)
        val body : 'a peg option ref = ref NONE
        (* memo array + the generation it belongs to; rebuilt when stale *)
        val memo : 'a entry option array ref = ref (Array.fromList [])
        val memoGen : int ref = ref ~1

        fun ensureMemo () =
            if !memoGen <> !generation
            then (memo := Array.array (inputSize () + 1, NONE);
                  memoGen := !generation)
            else ()

        fun getBody () =
            case !body of SOME b => b
                        | NONE => let val b = thunk () in body := SOME b; b end
      in
        fn pos =>
          let
            val () = ensureMemo ()
            val arr = !memo
          in
            case Array.sub (arr, pos) of
                SOME (Done r) => (hits := !hits + 1; r)
              | SOME InProgress => raise LeftRecursion name
              | NONE =>
                  let
                    val () = Array.update (arr, pos, SOME InProgress)
                    val r = (getBody ()) pos
                    val () = Array.update (arr, pos, SOME (Done r))
                  in r end
          end
      end

  (* ---- driver ---- *)

  fun parse p input =
      (theInput := input;
       generation := !generation + 1;
       hits := 0;
       p 0)

  fun memoHits () = !hits
end
