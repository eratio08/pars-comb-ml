(* It's missing the Functor and Applicative part. *)
(* module type Functor = sig *)
(*   type 'a t *)
(**)
(*   (** Applies the given function with the value of the given functor. *) *)
(*   val fmap : 'a 'b. ('a -> 'b) -> 'a t -> 'b t *)
(**)
(*   (** Lifts the first argument into a functor and ignores the second argument, points towards the value being kept. *) *)
(*   val ( <$ ) : 'a 'b. 'a -> 'b t -> 'a t *)
(**)
(*   (** Infix analog of [fmap] *) *)
(*   val ( <$> ) : 'a 'b. ('a -> 'b) -> 'a t -> 'b t *)
(**)
(*   (** The flipped version of [( <$ )], points towards the value being kept. *) *)
(*   val ( $> ) : 'a 'b. 'a t -> 'b -> 'b t *)
(**)
(*   (** Ignores the value. *) *)
(*   val void : 'a. 'a t -> unit t *)
(* end *)
(**)
(* module type Applicative = sig *)
(*   type 'a t *)
(**)
(*   include Functor with type 'a t := 'a t *)
(**)
(*   (** Lifts the argument into the computational context, effect free. *) *)
(*   val pure : 'a. 'a -> 'a t *)
(**)
(*   (** Empty computational context. *) *)
(*   val empty : 'a t *)
(**)
(*   (** Chaining operator, like a lifted version of [fmap] in a computational context. *) *)
(*   val ( <*> ) : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t *)
(**)
(*   (** Chaining operator. Keep the side it points to. *) *)
(*   val ( *> ) : 'a 'b. 'a t -> 'b t -> 'b t *)
(**)
(*   (** Chaining operator. Keep the side it points to. *) *)
(*   val ( <* ) : 'a 'b. 'a t -> 'b t -> 'a t *)
(**)
(*   (** Has the same signature of [fmap]. It mainly exists to parallel [lift_a_2] and [lift_a_3]. *) *)
(*   val lift_a : 'a 'b. ('a -> 'b) -> 'a t -> 'b t *)
(**)
(*   (** Lifts a two argument function into the computational context. *) *)
(*   val lift_a_2 : 'a 'b 'c. ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t *)
(**)
(*   (** Lifts a three argument function into the computational context. *) *)
(*   val lift_a_3 : 'a 'b 'c 'd. ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t *)
(**)
(*   (** Behaves like [<*>]. The results of the first computation are provided as input to the function of the second computation. *)
     (*       The signature might look like this is a case of [flip (<*>)] but this operator keep the order of operations. *) *)
(*   val ( <**> ) : 'a 'b. 'a t -> ('a -> 'b) t -> 'b t *)
(**)
(*   (** Conditionally executes a computation. *)
     (*       If the first argument is [true] the second computation is evaluate, otherwise the last computation is evaluated. *) *)
(*   val when_ : bool -> unit t -> unit t *)
(**)
(*   (** Behaves like [when_] but negated. *) *)
(*   val unless : bool -> unit t -> unit t *)
(**)
(*   (** Evaluate only of the first argument is [true]. *) *)
(*   val guard : bool -> unit t *)
(* end *)

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

  (* include Applicative with type 'a t := 'a t *)

  (** Lifts [`a] into the monad. *)
  val return : 'a -> 'a t

  (** Sequencing function for the monad, aka bind.
      Applies the results of the parser [t] to [f]. *)
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

  (** The zero parser fails for all inputs. *)
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

  (** Non-deterministic choice operator for parsers.
      Parser [t1] and [t2] are both applied to the argument string the results are appended in order of evaluation. *)
  val ( ++ ) : 'a t -> 'a t -> 'a t
end

module Parser : sig
  (** A parser is a function takes a string of characters and returns a list of results.
      Per convention an empty result list signals a failure.
      A non-empty list signals success.
      In the resulting pairs the first component is the type produced by parsing and processing the prefix string.
      The second component is the unparsed suffix of the argument string. *)
  type 'a t = Parser of (string -> ('a * string) list)

  include MonadPlus with type 'a t := 'a t

  (** Deconstructor helper to access the parser [p] inside the monad. *)
  val parse : 'a t -> string -> ('a * string) list

  (** A parser that consumes a single non-empty character. *)
  val item : char t

  (** Deterministic choice operator for parsers, behaves like [++] but only returns a single result.
      So parser [t1] and [t2] are both applied to the argument string the first evaluated result is returned.
      This operator is often known as [<|>]. *)
  val ( +++ ) : 'a t -> 'a t -> 'a t

  (** Conditional combinator. Yields a parser that consumes a character in case the predicate [p] is satisfied. *)
  val sat : (char -> bool) -> char t

  (** Parser of a specified character [c]. *)
  val char : char -> char t

  (** Parse a specified string. *)
  val string : string -> string t

  (** Repeats application of parser [t] zero or more times. *)
  val many : 'a t -> 'a list t

  (** Like [many], applies one or more times. *)
  val many_1 : 'a t -> 'a list t

  (** Repeats application of parser [t] separate by parser [sep].
      The results of parser [sep] are discarded.
      Matches zero or more times. *)
  val sep_by : 'a t -> 'b t -> 'a list t

  (** Like [sep_by]. Matches one or more times. *)
  val sep_by_1 : 'a t -> 'b t -> 'a list t

  (** Repeats application of parser [t] separated by application of parser [op].
      The result of [op] is assumed to be a left associative operator.
      The operator is used to combine the results of [p].
      Matches zero or more times, if matched zero times use fall back [a]. *)
  val chain_l : 'a t -> ('a -> 'a -> 'a) t -> 'a -> 'a t

  (** Like [chain_l]. Matches one or more times. *)
  val chain_l_1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t

  (** Repeats application op parser [t] separated by application of parser [op].
      The result of [op] is assumed to be a right associative operator.
      The operator is used to combine the results of [p].
      Matches zero or more times, if matched zero times use fall back [a]. *)
  val chain_r : 'a t -> ('a -> 'a -> 'a) t -> 'a -> 'a t

  (** Like [chain_r]. Matches one or more times. *)
  val chain_r_1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t

  (** Parses a string of spaces, tabs or newlines. *)
  val space : string t

  (** Parses a token and discards any trailing spaces. *)
  val token : 'a t -> 'a t

  (** Parses a symbolic token. *)
  val symb : string -> string t

  (** Applies parser [t] and discards leading spaces. *)
  val apply : 'a t -> string -> ('a * string) list
end = struct
  type 'a t = Parser of (string -> ('a * string) list)

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

  let return (a : 'a) : 'a t = Parser (fun cs -> [ a, cs ])
  let parse (Parser p : 'a t) : string -> ('a * string) list = p

  let ( >>= ) (t : 'a t) (f : 'a -> 'b t) : 'b t =
    Parser
      (fun cs -> List.concat (List.map (fun (a, cs') -> parse (f a) cs') (parse t cs)))
  ;;

  (* Choice combinators *)

  let zero : 'a t = Parser (fun _ -> [])

  let ( ++ ) (t1 : 'a t) (t2 : 'a t) : 'a t =
    Parser
      (fun cs ->
        let p = parse t1 cs in
        let q = parse t2 cs in
        List.append p q)
  ;;

  let ( +++ ) (t1 : 'a t) (t2 : 'a t) : 'a t =
    Parser
      (fun cs ->
        match parse (t1 ++ t2) cs with
        | [] -> []
        | x :: _ -> [ x ])
  ;;

  let sat (p : char -> bool) : char t = item >>= fun c -> if p c then return c else zero
  let char (c : char) : char t = sat (( = ) c)

  (* Recursive parser *)

  let rec string : string -> string t = function
    | "" -> return ""
    | cs ->
      let c = String.get cs 0 in
      let cs = String.sub cs 1 (String.length cs - 1) in
      char c >>= fun _ -> string cs >>= fun _ -> return (Char.escaped c ^ cs)
  ;;

  let rec many (t : 'a t) : 'a list t = many_1 t +++ return []

  and many_1 (t : 'a t) : 'a list t =
    t >>= fun a -> many t >>= fun as_ -> return (a :: as_)
  ;;

  let sep_by_1 (t : 'a t) (sep : 'b t) : 'a list t =
    t >>= fun a -> many (sep >>= fun _ -> t) >>= fun as_ -> return (a :: as_)
  ;;

  let sep_by (t : 'a t) (sep : 'b t) : 'a list t = sep_by_1 t sep +++ return []

  let chain_l_1 (t : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
    let rec rest (a : 'a) : 'a t =
      (op >>= fun f -> t >>= fun b -> rest (f a b)) +++ return a
    in
    t >>= fun a -> rest a
  ;;

  let chain_l (t : 'a t) (op : ('a -> 'a -> 'a) t) (a : 'a) : 'a t =
    chain_l_1 t op +++ return a
  ;;

  let rec chain_r_1 (t : 'a t) (op : ('a -> 'a -> 'a) t) : 'a t =
    let rec rest (a : 'a) : 'a t =
      (op >>= fun f -> chain_r_1 t op >>= fun b -> rest (f a b)) +++ return a
    in
    t >>= fun a -> rest a
  ;;

  let chain_r (t : 'a t) (op : ('a -> 'a -> 'a) t) (a : 'a) : 'a t =
    chain_r_1 t op +++ return a
  ;;

  (* Lexical combinators *)

  let space : string t =
    let is_space c = Char.equal c ' ' || Char.equal c '\t' || Char.equal c '\n' in
    let to_str cs = cs |> List.to_seq |> String.of_seq in
    many (sat is_space) >>= fun cs -> to_str cs |> return
  ;;

  let token (t : 'a t) : 'a t = t >>= fun a -> space >>= fun _ -> return a
  let symb (s : string) : string t = token (string s)
  let apply (t : 'a t) : string -> ('a * string) list = space >>= (fun _ -> t) |> parse
end
