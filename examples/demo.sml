(* demo.sml - a small arithmetic-expression PEG (with precedence and
   left-associative folding), plus terminals/combinators and memoization,
   run over fixed literal input strings. Deterministic: no I/O but print. *)

structure P = Packrat

val digitP = P.litc Char.isDigit

(* expr <- term (('+' / '-') term)*
   term <- factor (('*' / '/') factor)*
   factor <- number / '(' expr ')' *)
val number = P.map (fn ds => valOf (Int.fromString (implode ds))) (P.many1 digitP)
val addop  = P.alt (P.map (fn _ => op +) (P.chr #"+")) (P.map (fn _ => op -) (P.chr #"-"))
val mulop  = P.alt (P.map (fn _ => op * ) (P.chr #"*")) (P.map (fn _ => op div) (P.chr #"/"))
val exprRef : int P.peg ref = ref (P.map (fn _ => 0) (P.chr #"\000"))
val factor = P.rule "factor" (fn () =>
  P.alt number
        (P.seqR (P.chr #"(") (P.seqL (P.delay (fn () => !exprRef)) (P.chr #")"))))
val term = P.rule "term" (fn () =>
  P.map (fn (x, rest) => List.foldl (fn ((f, y), acc) => f (acc, y)) x rest)
        (P.seq factor (P.many (P.seq mulop factor))))
val expr = P.rule "expr" (fn () =>
  P.map (fn (x, rest) => List.foldl (fn ((f, y), acc) => f (acc, y)) x rest)
        (P.seq term (P.many (P.seq addop term))))
val () = exprRef := expr

fun calc s = P.parse (P.seqL expr (P.notP P.anyc)) s
fun showCalc s =
  case calc s of
      P.Ok (v, _) => print ("  " ^ s ^ "  =  " ^ Int.toString v ^ "\n")
    | P.Err pos => print ("  " ^ s ^ "  -> parse error at " ^ Int.toString pos ^ "\n")

val () = print "=== sml-packrat demo ===\n\n"
val () = print "Arithmetic PEG (expr <- term (('+'/'-') term)*, with precedence):\n"
val () = app showCalc ["2+3*4", "(2+3)*4", "10-3-2", "2*(3+4*(5-1))"]

val () = print "\nTerminals and ordered choice:\n"
val () = print ("  chr #\"a\" on \"abc\"   -> "
                ^ (case P.parse (P.chr #"a") "abc" of P.Ok (c, n) => str c ^ " @" ^ Int.toString n
                                                      | P.Err n => "err @" ^ Int.toString n) ^ "\n")
val () = print ("  str \"let\" on \"lex\"  -> "
                ^ (case P.parse (P.str "let") "lex" of P.Ok (v, n) => v ^ " @" ^ Int.toString n
                                                       | P.Err n => "err @" ^ Int.toString n) ^ "\n")

val () = print "\nMemoization (shared-prefix ordered choice):\n"
val aaa = P.map (fn _ => ()) (P.seq (P.chr #"a") (P.seq (P.chr #"a") (P.chr #"a")))
val ruleA = P.rule "A" (fn () => aaa)
val ruleS = P.rule "S" (fn () =>
  P.alt (P.seqL ruleA (P.chr #"c")) (P.seqL ruleA (P.chr #"d")))
val resultS = P.parse ruleS "aaad"
val () = print ("  S <- A 'c' / A 'd'  on \"aaad\"  -> "
                ^ (case resultS of P.Ok (_, n) => "ok @" ^ Int.toString n | P.Err n => "err @" ^ Int.toString n)
                ^ "\n")
val () = print ("  memo hits so far    = " ^ Int.toString (P.memoHits ()) ^ "\n")

val () = print "\nLeft recursion is detected, not looped forever:\n"
val lRef : char P.peg ref = ref (P.chr #"\000")
val l = P.rule "L" (fn () =>
  P.alt (P.seqL (P.delay (fn () => !lRef)) (P.chr #"a")) (P.chr #"a"))
val () = lRef := l
val () = print ("  L <- L 'a' / 'a'  on \"aaa\"  -> "
                ^ ((ignore (P.parse l "aaa"); "no exception (unexpected)")
                   handle P.LeftRecursion name => "LeftRecursion \"" ^ name ^ "\"")
                ^ "\n")
