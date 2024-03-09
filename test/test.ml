let () =
  Alcotest.run
    "Parser Combinator"
    [ "Parser Atoms", Test_parser.suite; "Arithmetic Expression", Test_arithmetic.suite ]
;;
