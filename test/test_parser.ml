let test_item () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.CharParser.testable)
    "item without rest"
    [ 'a', "" ]
    (parse item "a");
  Alcotest.(check Util.CharParser.testable)
    "item with rest"
    [ 'a', "bc" ]
    (parse item "abc")
;;

let test_zero () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.CharParser.testable) "zero without rest" [] (parse zero "a");
  Alcotest.(check Util.CharParser.testable) "zero with rest" [] (parse zero "abc")
;;

let test_bind () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.CharParser.testable)
    "bind with char shifted by one"
    [ 'b', "" ]
    (parse (item >>= fun c -> c |> Char.code |> ( + ) 1 |> Char.chr |> return) "a")
;;

let test_sat () =
  let open Pars_comb.Parser in
  let is_digit c =
    let ascii = Char.code c in
    ascii >= 48 && ascii <= 57
  in
  Alcotest.(check Util.CharParser.testable)
    "sat matching number without rest"
    [ '1', "" ]
    (parse (sat is_digit) "1");
  Alcotest.(check Util.CharParser.testable)
    "sat matching number with rest"
    [ '1', " 2 3" ]
    (parse (sat is_digit) "1 2 3")
;;

let test_char () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.CharParser.testable)
    "char matching"
    [ 'a', "" ]
    (parse (char 'a') "a");
  Alcotest.(check Util.CharParser.testable)
    "char matching multiple chars"
    [ 'a', "b" ]
    (parse (char 'a') "ab");
  Alcotest.(check Util.CharParser.testable) "char not matching" [] (parse (char 'a') "b");
  Alcotest.(check Util.CharParser.testable)
    "char not matching multiple chars"
    []
    (parse (char 'a') "ba")
;;

let test_string () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringParser.testable)
    "string single char without rest"
    [ "a", "" ]
    (parse (string "a") "a");
  Alcotest.(check Util.StringParser.testable)
    "string multiple chars char without rest"
    [ "ab", "" ]
    (parse (string "ab") "ab");
  Alcotest.(check Util.StringParser.testable)
    "string single char with rest"
    [ "a", "b" ]
    (parse (string "a") "ab");
  Alcotest.(check Util.StringParser.testable)
    "string multiple chars char with rest"
    [ "ab", "c" ]
    (parse (string "ab") "abc")
;;

let test_choice () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.CharParser.testable)
    "choice matching first parser"
    [ 'a', "" ]
    (parse (char 'a' ++ char 'b') "a");
  Alcotest.(check Util.CharParser.testable)
    "choice matching first parser"
    [ 'b', "" ]
    (parse (char 'a' ++ char 'b') "b");
  Alcotest.(check Util.CharParser.testable)
    "choice matching first parser with rest"
    [ 'a', "b" ]
    (parse (char 'a' ++ char 'b') "ab");
  Alcotest.(check Util.CharParser.testable)
    "choice matching first parser with rest"
    [ 'b', "a" ]
    (parse (char 'a' ++ char 'b') "ba");
  Alcotest.(check Util.StringParser.testable)
    "choice matching first and second parser"
    [ "a", "b"; "ab", "" ]
    (parse (string "a" ++ string "ab") "ab")
;;

let test_determenistic_choice () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.CharParser.testable)
    "choice matching first parser"
    [ 'a', "" ]
    (parse (char 'a' +++ char 'b') "a");
  Alcotest.(check Util.CharParser.testable)
    "choice matching first parser"
    [ 'b', "" ]
    (parse (char 'a' +++ char 'b') "b");
  Alcotest.(check Util.CharParser.testable)
    "choice matching first parser with rest"
    [ 'a', "b" ]
    (parse (char 'a' +++ char 'b') "ab");
  Alcotest.(check Util.CharParser.testable)
    "choice matching first parser with rest"
    [ 'b', "a" ]
    (parse (char 'a' +++ char 'b') "ba");
  Alcotest.(check Util.StringParser.testable)
    "choice matching first and second parser only keeping first match"
    [ "a", "b" ]
    (parse (string "a" +++ string "ab") "ab");
  Alcotest.(check Util.StringParser.testable)
    "choice match first matching parser"
    [ "abc", "" ]
    (parse (string "ac" +++ string "cb" +++ string "abc") "abc")
;;

let test_many () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringListParser.testable)
    "many match single char string 3 times"
    [ [ "a"; "a"; "a" ], " bb cc" ]
    (parse (many (string "a")) "aaa bb cc");
  Alcotest.(check Util.StringListParser.testable)
    "many match none"
    [ [], " aaa bb cc" ]
    (parse (many (string "a")) " aaa bb cc")
;;

let test_many_1 () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringListParser.testable)
    "many_1 match single char string 3 times"
    [ [ "a"; "a"; "a" ], " bb cc" ]
    (parse (many_1 (string "a")) "aaa bb cc");
  Alcotest.(check Util.StringListParser.testable)
    "many_1 fail parser"
    []
    (parse (many_1 (string "a")) " aaa bb cc")
;;

let test_sep_by () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringListParser.testable)
    "sep_by match one but not the separator"
    [ [ "a" ], ", bb cc" ]
    (parse (sep_by (string "a") (string ",")) "a, bb cc");
  Alcotest.(check Util.StringListParser.testable)
    "sep_by match twice"
    [ [ "a"; "a" ], " bb cc" ]
    (parse (sep_by (string "a") (string ",")) "a,a bb cc");
  Alcotest.(check Util.StringListParser.testable)
    "sep_by match 3"
    [ [ "a"; "a"; "a" ], " bb cc" ]
    (parse (sep_by (string "a") (string ",")) "a,a,a bb cc");
  Alcotest.(check Util.StringListParser.testable)
    "sep_by match none"
    [ [], "ba,a,a bb cc" ]
    (parse (sep_by (string "a") (string ",")) "ba,a,a bb cc")
;;

let test_sep_by_1 () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringListParser.testable)
    "sep_by_1 match one but not the separator"
    [ [ "a" ], ", bb cc" ]
    (parse (sep_by_1 (string "a") (string ",")) "a, bb cc");
  Alcotest.(check Util.StringListParser.testable)
    "sep_by_1 match twice"
    [ [ "a"; "a" ], " bb cc" ]
    (parse (sep_by_1 (string "a") (string ",")) "a,a bb cc");
  Alcotest.(check Util.StringListParser.testable)
    "sep_by_1 match 3"
    [ [ "a"; "a"; "a" ], " bb cc" ]
    (parse (sep_by_1 (string "a") (string ",")) "a,a,a bb cc");
  Alcotest.(check Util.StringListParser.testable)
    "sep_by_1 fail parser"
    []
    (parse (sep_by_1 (string "a") (string ",")) "ba,a,a bb cc")
;;

let test_chain_l () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringParser.testable)
    "chain_l"
    [ "aaaa", "" ]
    (parse (chain_l (string "a") (return ( ^ )) "x") "aaaa");
  Alcotest.(check Util.StringParser.testable)
    "chain_l fallback"
    [ "x", "bbbb" ]
    (parse (chain_l (string "a") (return ( ^ )) "x") "bbbb")
;;

let test_chain_l_1 () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringParser.testable)
    "chain_l"
    [ "aaaa", "" ]
    (parse (chain_l_1 (string "a") (return ( ^ ))) "aaaa");
  Alcotest.(check Util.StringParser.testable)
    "chain_l fail parser"
    []
    (parse (chain_l_1 (string "a") (return ( ^ ))) "bbbb");
  Alcotest.(check Util.StringParser.testable)
    "chain_l with parsing operator"
    [ "aaa", "" ]
    (parse (chain_l_1 (string "a") (string "b" >>= fun _ -> return ( ^ ))) "ababa");
  Alcotest.(check Util.StringParser.testable)
    "chain_l only match of left and right argument of the operator are pressent"
    [ "aa", "b" ]
    (parse (chain_l_1 (string "a") (string "b" >>= fun _ -> return ( ^ ))) "abab")
;;

let test_space () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringParser.testable)
    "space match single space"
    [ " ", "" ]
    (parse space " ");
  Alcotest.(check Util.StringParser.testable)
    "space match multiple spaces"
    [ "    ", "" ]
    (parse space "    ");
  Alcotest.(check Util.StringParser.testable)
    "space match tabs"
    [ "\t\t", "" ]
    (parse space "\t\t");
  Alcotest.(check Util.StringParser.testable)
    "space match new-lines"
    [ "\n\n", "" ]
    (parse space {|

|})
;;

let test_token () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringParser.testable)
    "token with trailing spaces"
    [ "a", "" ]
    (parse (token (string "a")) "a   ");
  Alcotest.(check Util.StringParser.testable)
    "token without tailing spaces"
    [ "a", "" ]
    (parse (token (string "a")) "a");
  Alcotest.(check Util.StringParser.testable)
    "token with tailing spaces and char"
    [ "a", "b" ]
    (parse (token (string "a")) "a    b")
;;

let test_symb () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringParser.testable)
    "symb with trailing spaces and char"
    [ "abc", "d" ]
    (parse (symb "abc") "abc   d");
  Alcotest.(check Util.StringParser.testable)
    "symb with trailing spaces"
    [ "abc", "" ]
    (parse (symb "abc") "abc   ");
  Alcotest.(check Util.StringParser.testable)
    "symb without trailing spaces"
    [ "abc", "" ]
    (parse (symb "abc") "abc")
;;

let test_apply () =
  let open Pars_comb.Parser in
  Alcotest.(check Util.StringParser.testable)
    "apply with leading and trailing spaces"
    [ "abc", "" ]
    (apply (symb "abc") "    abc    ");
  Alcotest.(check Util.StringParser.testable)
    "apply with leading spaces"
    [ "abc", "" ]
    (apply (symb "abc") "    abc");
  Alcotest.(check Util.StringParser.testable)
    "apply without leading and trailing spaces"
    [ "abc", "" ]
    (apply (symb "abc") "abc");
  Alcotest.(check Util.StringParser.testable)
    "apply not matching with leading and trailing spaces"
    []
    (apply (symb "abc") "  bc  ")
;;

let suite : unit Alcotest.test_case list =
  [ "Parser.item", `Quick, test_item
  ; "Parser.zero", `Quick, test_zero
  ; "Parser.(>>=)", `Quick, test_bind
  ; "Parser.sat", `Quick, test_sat
  ; "Parser.char", `Quick, test_char
  ; "Parser.string", `Quick, test_string
  ; "Parser.(++)", `Quick, test_choice
  ; "Parser.(+++)", `Quick, test_determenistic_choice
  ; "Parser.many", `Quick, test_many
  ; "Parser.many_1", `Quick, test_many_1
  ; "Parser.sep_by", `Quick, test_sep_by
  ; "Parser.sep_by_1", `Quick, test_sep_by_1
  ; "Parser.chain_l", `Quick, test_chain_l
  ; "Parser.chain_l_1", `Quick, test_chain_l_1
  ; "Parser.space", `Quick, test_space
  ; "Parser.token", `Quick, test_token
  ; "Parser.symb", `Quick, test_symb
  ; "Parser.apply", `Quick, test_apply
  ]
;;
