(* packrat.sig

   A memoizing PEG (Parsing Expression Grammar) parser for Standard ML.

   PEGs use *ordered* choice and are unambiguous; packrat parsing memoizes each
   rule's result at each input position so that the whole parse runs in time
   linear in the input length, even for grammars that would otherwise backtrack
   exponentially.

   Each `rule` owns its own per-position memo table. The table is reset
   automatically at the start of every `parse`, so rules are reusable across
   parses and across inputs.

   Combinators implement the PEG operators:
     seq   e1 e2     sequence
     alt   e1 e2     ordered choice (try e1, then e2)
     many  e         greedy zero-or-more (zero or more e)
     many1 e         greedy one-or-more (one or more e)
     opt   e         optional (zero or one e)
     andP  e         and-predicate (&e): succeed iff e matches, consume nothing
     notP  e         not-predicate (!e): succeed iff e fails, consume nothing *)

signature PACKRAT =
sig
  type 'a peg

  datatype 'a result = Ok of 'a * int   (* value and next position *)
                     | Err of int        (* furthest failure position *)

  (* Terminals. *)
  val chr    : char -> char peg
  val anyc   : char peg                       (* any single character        *)
  val litc   : (char -> bool) -> char peg      (* a char matching a predicate  *)
  val str    : string -> string peg            (* an exact string             *)

  (* Combinators. *)
  val seq    : 'a peg -> 'b peg -> ('a * 'b) peg
  val alt    : 'a peg -> 'a peg -> 'a peg
  val map    : ('a -> 'b) -> 'a peg -> 'b peg
  val many   : 'a peg -> 'a list peg
  val many1  : 'a peg -> 'a list peg
  val opt    : 'a peg -> 'a option peg
  val andP   : 'a peg -> unit peg              (* &e lookahead                 *)
  val notP   : 'a peg -> unit peg              (* !e negative lookahead        *)
  val seqL   : 'a peg -> 'b peg -> 'a peg       (* keep left                    *)
  val seqR   : 'a peg -> 'b peg -> 'b peg       (* keep right                   *)
  val ret    : 'a -> 'a peg                     (* succeed, consume nothing     *)

  (* A named, memoized rule. Pass a thunk so recursive grammars can refer to
     rules defined later. `name` is used only in diagnostics. *)
  val rule   : string -> (unit -> 'a peg) -> 'a peg

  (* Defer construction of a peg until it is run. Useful for tying recursive
     grammar knots through a ref when the peg type is abstract. *)
  val delay  : (unit -> 'a peg) -> 'a peg

  (* Run a peg over a string. Resets all rule memo tables first. *)
  val parse  : 'a peg -> string -> 'a result

  (* Total number of memoized lookups that were served from a memo table
     (a cache hit) during the most recent `parse`. Useful for verifying that
     memoization is actually happening. *)
  val memoHits : unit -> int

  exception LeftRecursion of string
end
