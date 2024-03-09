module IntParser = struct
  let pp_pair fmt = function
    | i, cs -> Format.fprintf fmt "(%d, \"%s\")" i cs
  ;;

  let pp fmt = function
    | [] -> Format.fprintf fmt "[]"
    | p ->
      Format.fprintf fmt "%a" Fmt.(brackets (list ~sep:semi (fun fmt -> pp_pair fmt))) p
  ;;

  let testable = Alcotest.testable pp ( = )
end

module CharParser = struct
  let pp_pair fmt = function
    | i, cs -> Format.fprintf fmt "('%c', \"%s\")" i cs
  ;;

  let pp fmt = function
    | [] -> Format.fprintf fmt "[]"
    | p ->
      Format.fprintf fmt "%a" Fmt.(brackets (list ~sep:semi (fun fmt -> pp_pair fmt))) p
  ;;

  let testable = Alcotest.testable pp ( = )
end

module StringParser = struct
  let pp_pair fmt = function
    | i, cs -> Format.fprintf fmt "(\"%s\", \"%s\")" i cs
  ;;

  let pp fmt = function
    | [] -> Format.fprintf fmt "[]"
    | p ->
      Format.fprintf fmt "%a" Fmt.(brackets (list ~sep:semi (fun fmt -> pp_pair fmt))) p
  ;;

  let testable = Alcotest.testable pp ( = )
end

module StringListParser = struct
  let pp_pair fmt = function
    | i, cs ->
      Format.fprintf fmt "(%a, \"%s\")" Fmt.(brackets (list ~sep:semi string)) i cs
  ;;

  let pp fmt = function
    | [] -> Format.fprintf fmt "[]"
    | p ->
      Format.fprintf fmt "%a" Fmt.(brackets (list ~sep:semi (fun fmt -> pp_pair fmt))) p
  ;;

  let testable = Alcotest.testable pp ( = )
end
