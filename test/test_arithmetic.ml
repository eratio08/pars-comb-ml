(* For testing implement the following grammar

   expr ::= expr addop term | term
   term ::= term mulop factor | factor
   factor ::= digit | ( expr )
   digit ::= 0 | 1 | . . . | 9
   addop ::= + | -
   mulop ::= * | /
*)

module ArithmeticExpressionParser = struct
  open Pars_comb.Parser

  (** Parse ["+"] or ["-"] and return the respective OCaml function for [+] or [-]. *)
  let add_op : (int -> int -> int) t =
    (symb "+" >>= fun _ -> return ( + )) +++ (symb "-" >>= fun _ -> return ( - ))
  ;;

  (** Parse ["*"] or ["/"] and return the respective OCaml function for [*] or [/]. *)
  let mul_op : (int -> int -> int) t =
    (symb "*" >>= fun _ -> return ( * )) +++ (symb "/" >>= fun _ -> return ( / ))
  ;;

  (** Parses single digit characters and returns the respective int by subtracting the code point from code point of 0. *)
  let digit : int t =
    let is_digit c =
      let ascii = Char.code c in
      ascii >= 48 && ascii <= 57
    in
    token (sat is_digit) >>= fun x -> return (Char.code x - Char.code '0')
  ;;

  (* TODO Find a way around this unit-quirk which seems to be required in mutual-recursive declarations. *)
  let rec expr (() : unit) : int t = chain_l_1 (term ()) add_op

  (** Parses a factor that is followed by ["*"] or ["/"]. *)
  and term (() : unit) : int t = chain_l_1 (factor ()) mul_op

  (** Parses a digit or an expression that is enclosed in '(' and ')' returning the resulting integer. *)
  and factor (() : unit) : int t =
    digit +++ (symb "(" >>= fun _ -> expr () >>= fun n -> symb ")" >>= fun _ -> return n)
  ;;
end

let test_simple_expression () =
  let open Pars_comb.Parser in
  let open ArithmeticExpressionParser in
  Alcotest.(check Util.IntParser.testable)
    "should return -1"
    [ -1, "" ]
    (apply (expr ()) " 1 - 2 * 3 + 4 ")
;;

let suite : unit Alcotest.test_case list = [ "simple", `Quick, test_simple_expression ]
