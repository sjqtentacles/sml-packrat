(* Dependency-free test runner for the Packrat structure.
 * Prints one line per assertion and exits non-zero if any assertion fails. *)

structure P = Packrat

val passed = ref 0
val failed = ref 0

fun check (name : string) (cond : bool) : unit =
    if cond
    then (passed := !passed + 1; print ("ok   - " ^ name ^ "\n"))
    else (failed := !failed + 1; print ("FAIL - " ^ name ^ "\n"))

fun isOk (P.Ok _) = true | isOk _ = false
fun isErr (P.Err _) = true | isErr _ = false
fun fullParse (r, n) = case r of P.Ok (_, pos) => pos = n | P.Err _ => false
fun okVal eq (r, expected) =
    case r of P.Ok (v, _) => eq (v, expected) | P.Err _ => false

val digitP = P.litc Char.isDigit

fun run () =
  let
    (* terminals *)
    val () = check "chr matches" (isOk (P.parse (P.chr #"a") "abc"))
    val () = check "chr fails" (isErr (P.parse (P.chr #"a") "xyz"))
    val () = check "str matches" (okVal (op =) (P.parse (P.str "let") "let x", "let"))
    val () = check "str fails atomically" (isErr (P.parse (P.str "let") "lex"))

    (* many / many1 *)
    val digits = P.map implode (P.many1 digitP)
    val () = check "many1 digits"
                   (okVal (op =) (P.parse digits "12345", "12345"))
    val () = check "many1 needs one" (isErr (P.parse (P.many1 digitP) "abc"))
    val () = check "many allows zero"
                   (okVal (op =) (P.parse (P.map implode (P.many digitP)) "abc", ""))

    (* ordered choice *)
    val ab = P.alt (P.chr #"a") (P.chr #"b")
    val () = check "alt first" (okVal (op =) (P.parse ab "a", #"a"))
    val () = check "alt second" (okVal (op =) (P.parse ab "b", #"b"))

    (* andP / notP lookahead.
       Identifier = letter, but NOT the keyword "if" followed by a non-letter.
       Here we just exercise the predicates directly. *)
    val () = check "andP consumes nothing on success"
                   (case P.parse (P.seq (P.andP (P.chr #"a")) (P.chr #"a")) "a" of
                        P.Ok (_, pos) => pos = 1 | _ => false)
    val () = check "andP fails when inner fails"
                   (isErr (P.parse (P.andP (P.chr #"a")) "b"))
    val () = check "notP succeeds when inner fails, consumes nothing"
                   (case P.parse (P.notP (P.chr #"a")) "b" of
                        P.Ok (_, pos) => pos = 0 | _ => false)
    val () = check "notP fails when inner succeeds"
                   (isErr (P.parse (P.notP (P.chr #"a")) "a"))

    (* A complete arithmetic PEG with precedence and left-assoc folding.
       expr   <- term (('+' / '-') term)*
       term   <- factor (('*' / '/') factor)*
       factor <- number / '(' expr ')'
       Each rule is constructed exactly once (so its memo table is shared);
       recursive references go through the rule values captured in the thunks. *)
    val number = P.map (fn ds => valOf (Int.fromString (implode ds)))
                       (P.many1 digitP)
    val addop = P.alt (P.map (fn _ => op +) (P.chr #"+"))
                      (P.map (fn _ => op -) (P.chr #"-"))
    val mulop = P.alt (P.map (fn _ => op * ) (P.chr #"*"))
                      (P.map (fn _ => op div) (P.chr #"/"))
    val exprRef : int P.peg ref = ref (P.map (fn _ => 0) (P.chr #"\000"))
    val factor = P.rule "factor" (fn () =>
      P.alt number
            (P.seqR (P.chr #"(") (P.seqL (P.delay (fn () => !exprRef)) (P.chr #")"))))
    val term = P.rule "term" (fn () =>
      P.map (fn (x, rest) =>
               List.foldl (fn ((f, y), acc) => f (acc, y)) x rest)
        (P.seq factor (P.many (P.seq mulop factor))))
    val expr = P.rule "expr" (fn () =>
      P.map (fn (x, rest) =>
               List.foldl (fn ((f, y), acc) => f (acc, y)) x rest)
        (P.seq term (P.many (P.seq addop term))))
    val () = exprRef := expr

    fun calc s = P.parse (P.seqL expr (P.notP P.anyc)) s
    fun calcVal s = case calc s of P.Ok (v, _) => SOME v | P.Err _ => NONE

    val () = check "PEG eval 2+3*4 = 14" (calcVal "2+3*4" = SOME 14)
    val () = check "PEG eval (2+3)*4 = 20" (calcVal "(2+3)*4" = SOME 20)
    val () = check "PEG eval left-assoc 10-3-2 = 5" (calcVal "10-3-2" = SOME 5)
    val () = check "PEG eval nested 2*(3+4*(5-1)) = 38" (calcVal "2*(3+4*(5-1))" = SOME 38)
    val () = check "PEG rejects trailing garbage" (calcVal "1+2)" = NONE)

    (* Memoization actually happens. PEG ordered choice re-tries alternatives
       from the same position; when both alternatives begin with the same
       sub-rule, the second attempt is served from the memo.
         S <- A 'c' / A 'd'   where   A <- 'a' 'a' 'a'
       On "aaad": first alt parses A (positions 0..3) then expects 'c', fails;
       second alt re-enters A at position 0 -> a memo hit -> then 'd' succeeds. *)
    val aaa = P.map (fn _ => ()) (P.seq (P.chr #"a") (P.seq (P.chr #"a") (P.chr #"a")))
    val ruleA = P.rule "A" (fn () => aaa)
    val ruleS = P.rule "S" (fn () =>
                  P.alt (P.seqL ruleA (P.chr #"c"))
                        (P.seqL ruleA (P.chr #"d")))
    val () = check "memoized shared sub-rule parses second alternative"
                   (isOk (P.parse ruleS "aaad"))
    val () = check "memoization records cache hits" (P.memoHits () > 0)

    (* A grammar whose naive (non-memoized) form is exponential. The classic:
         A <- B B 'x' / B B 'y'   ;   B <- 'a' / ''(empty)
       But to keep it simple and clearly memo-driven we reuse the shared-prefix
       shape at larger scale: a chain of choices that all start by parsing the
       same long prefix rule P, so each retry hits the memo.
         top <- P 'x' / P 'y'   ;   P <- 'a'*  (as a named rule) *)
    val astar = P.map (fn _ => ()) (P.many (P.chr #"a"))
    val ruleP = P.rule "P" (fn () => astar)
    val top = P.rule "top" (fn () =>
                P.alt (P.seqL ruleP (P.chr #"x"))
                      (P.seqL ruleP (P.chr #"y")))
    val longA = String.implode (List.tabulate (5000, fn _ => #"a")) ^ "y"
    val () = check "shared-prefix grammar parses large input (memoized)"
                   (isOk (P.parse top longA))
    val () = check "shared-prefix grammar used the memo"
                   (P.memoHits () > 0)

    (* Left recursion is detected rather than looping forever.
       L <- L 'a' / 'a'   -- direct left recursion. *)
    val lRef : char P.peg ref = ref (P.chr #"\000")
    val l = P.rule "L" (fn () =>
              P.alt (P.seqL (P.delay (fn () => !lRef)) (P.chr #"a")) (P.chr #"a"))
    val () = lRef := l
    val () = check "direct left recursion is detected"
                   ((ignore (P.parse l "aaa"); false)
                    handle P.LeftRecursion _ => true)
  in
    print ("\n" ^ Int.toString (!passed) ^ " passed, "
           ^ Int.toString (!failed) ^ " failed\n");
    OS.Process.exit (if !failed = 0 then OS.Process.success else OS.Process.failure)
  end

val () = run ()
