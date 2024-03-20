(* It's missing the Functor and Applicative part. *)
(** A Monad's operations affirm to the following laws
    {[
      let () =
        (* Left identity *)
        return a >>= f = f a;
        (* Right identity *)
        p >>= return = p;
        (* Associativity *)
        p >>= (fun a -> f a >>= g) = (p >>= fun a -> f a) >>= g
      ;;
    ]} *)
module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(** The following laws apply
    {[
      let () =
        let const x _ = x in
        (* Left identity *)
        zero >>= f = zero;
        (* Right identity *)
        p >>= const zero = zero
      ;;
    ]}*)
module type MonadZero = sig
  include Monad

  val zero : 'a t
end

(** The following laws apply
    {[
      let () =
        (* Distributivity *)
        p ++ q >>= f = (p >>= f) ++ (q >>= f);
        p >>= (fun a -> f a ++ g a) = (p >>= f) ++ (p >>= g)
      ;;
    ]}*)
module type MonadPlus = sig
  include MonadZero

  val ( ++ ) : 'a t -> 'a t -> 'a t
end

module Parser : sig
  type 'a t = Parser of (string -> ('a * string) list)

  include MonadPlus with type 'a t := 'a t

  val parse : 'a t -> string -> ('a * string) list
  val item : char t
  val ( +++ ) : 'a t -> 'a t -> 'a t
  val sat : (char -> bool) -> char t
  val char : char -> char t
  val string : string -> string t
  val many : 'a t -> 'a list t
  val many_1 : 'a t -> 'a list t
  val sep_by : 'a t -> 'b t -> 'a list t
  val sep_by_1 : 'a t -> 'b t -> 'a list t
  val chain_l : 'a t -> ('a -> 'a -> 'a) t -> 'a -> 'a t
  val chain_l_1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
  val chain_r : 'a t -> ('a -> 'a -> 'a) t -> 'a -> 'a t
  val chain_r_1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
  val space : string t
  val token : 'a t -> 'a t
  val symb : string -> string t
  val apply : 'a t -> string -> ('a * string) list
end = struct
  (** A parser is a function takes a string of characters and returns a list of results.
      Per convention an empty result list signals a failure.
      A non-empty list signals success.
      In the resulting pairs the first component is the type produced by parsing and processing the prefix string.
      The second component is the unparsed suffix of the argument string. *)
  type 'a t = Parser of (string -> ('a * string) list)

  (** A parser that consumes a single non-empty character. *)
  let item : char t =
    Parser
      (fun cs ->
        match cs with
        | "" -> []
        | cs ->
          let c = String.get cs 0 in
          let cs = String.sub cs 1 (String.length cs - 1) in
          [ c, cs ])
  ;;

  (* Monadic parser *)

  (** Lifts a into the monad. *)
  let return (a : 'a) : 'a t = Parser (fun cs -> [ a, cs ])

  (** Deconstructor helper to access the parser [p] inside the monad. *)
  let parse (Parser p : 'a t) : string -> ('a * string) list = p

  (** Sequencing function for the monad, aka bind.
      Applies the results of the parser [t] to [f]. *)
  let ( >>= ) (t : 'a t) (f : 'a -> 'b t) : 'b t =
    Parser
      (fun cs -> List.concat (List.map (fun (a, cs') -> parse (f a) cs') (parse t cs)))
  ;;

  (* Choice combinators *)

  (** The zero parser fails for all inputs. *)
  let zero : 'a t = Parser (fun _ -> [])

  (** Non-deterministic choice operator for parsers.
      Parser [t1] and [t2] are both applied to the argument string the results are appended in order of evaluation. *)
  let ( ++ ) (t1 : 'a t) (t2 : 'a t) : 'a t =
    Parser
      (fun cs ->
        let p = parse t1 cs in
        let q = parse t2 cs in
        List.append p q)
  ;;

  (** Deterministic choice operator for parsers, behaves like [++] but only returns a single result. *)
  let ( +++ ) (t1 : 'a t) (t2 : 'a t) : 'a t =
    Parser
      (fun cs ->
        match parse (t1 ++ t2) cs with
        | [] -> []
        | x :: _ -> [ x ])
  ;;

  (** Conditional combinator. Yields a parser that consumes a character in case the predicate [p] is satisfied. *)
  let sat (p : char -> bool) : char t = item >>= fun c -> if p c then return c else zero

  (** Parser of a specified character [c]. *)
  let char (c : char) : char t = sat (( = ) c)

  (* Recursive parser *)

  (** Parse a specified string. *)
  let rec string : string -> string t = function
    | "" -> return ""
    | cs ->
      let c = String.get cs 0 in
      let cs = String.sub cs 1 (String.length cs - 1) in
      char c >>= fun _ -> string cs >>= fun _ -> return (Char.escaped c ^ cs)
  ;;

  (** Repeats application of parser [t] zero or more times. *)
  let rec many (t : 'a t) : 'a list t = many_1 t +++ return []

  (** Like [many], applies one or more times. *)
  and many_1 (t : 'a t) : 'a list t =
    t >>= fun a -> many t >>= fun as_ -> return (a :: as_)
  ;;

  (** Like [sep_by]. Matches one or more times. *)
  let sep_by_1 (t : 'a t) (sep : 'b t) : 'a list t =
    t >>= fun a -> many (sep >>= fun _ -> t) >>= fun as_ -> return (a :: as_)
  ;;

  (** Repeats application of parser [t] separate by parser [sep].
      The results of parser [sep] are discarded.
      Matches zero or more times. *)
  let sep_by (t : 'a t) (sep : 'b t) : 'a list t = sep_by_1 t sep +++ return []

  (** Like [chain_l]. Matches one or more times. *)
  let chain_l_1 (t : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
    let rec rest (a : 'a) : 'a t =
      (op >>= fun f -> t >>= fun b -> rest (f a b)) +++ return a
    in
    t >>= fun a -> rest a
  ;;

  (** Repeats application op parser [t] separated by application of parser [op].
      The result of [op] is assumed to be a left associative operator.
      The operator is used to combine the results of [p].
      Matches zero or more times, if matched zero times use fall back [a]. *)
  let chain_l (t : 'a t) (op : ('a -> 'a -> 'a) t) (a : 'a) : 'a t =
    chain_l_1 t op +++ return a
  ;;

  (** Like [chain_r]. Matches one or more times. *)
  let rec chain_r_1 (t : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
    let rec rest (a : 'a) : 'a t =
      (op >>= fun f -> chain_r_1 t op >>= fun b -> rest (f a b)) +++ return a
    in
    t >>= fun a -> rest a
  ;;

  (** Repeats application op parser [t] separated by application of parser [op].
      The result of [op] is assumed to be a right associative operator.
      The operator is used to combine the results of [p].
      Matches zero or more times, if matched zero times use fall back [a]. *)
  let chain_r (t : 'a t) (op : ('a -> 'a -> 'a) t) (a : 'a) : 'a t =
    chain_r_1 t op +++ return a
  ;;

  (* Lexical combinators *)

  (** Parses a string of spaces, tabs or newlines. *)
  let space : string t =
    let is_space c = Char.equal c ' ' || Char.equal c '\t' || Char.equal c '\n' in
    let to_str cs = cs |> List.to_seq |> String.of_seq in
    many (sat is_space) >>= fun cs -> to_str cs |> return
  ;;

  (** Parses a token and discards any trailing spaces. *)
  let token (t : 'a t) : 'a t = t >>= fun a -> space >>= fun _ -> return a

  (** Parses a symbolic token. *)
  let symb (s : string) : string t = token (string s)

  (** Applies parser [t] and discards leading spaces. *)
  let apply (t : 'a t) : string -> ('a * string) list = space >>= (fun _ -> t) |> parse
end
