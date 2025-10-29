open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if binary operators and their
    operands do not have the correct types. *)
let bop_err = "Operator and operand type mismatch"

(** The error message produced if the guard
    of an [if] does not have type [bool]. *)
let if_guard_err = "Guard of if must have type bool"

(** [string_of_val v] converts a value [v] to a string. 
    Requires: [v] is a value. *)
let string_of_val (v : expr) : string =
  match v with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Int _ -> true
  | Bool _ -> true
  | _ -> false

(** [subst x v e] is [e] with all free occurrences of [x] replaced by [v]. *)
let rec subst (x : string) (v : expr) (e : expr) : expr =
  match e with
  | Int _ | Bool _ -> e
  | Var y -> if x = y then v else e
  | Binop (b, e1, e2) -> Binop (b, subst x v e1, subst x v e2)
  | Let (y, e1, e2) ->
      let e1' = subst x v e1 in
      if x = y then Let (y, e1', e2) else Let (y, e1', subst x v e2)
  | If (e1, e2, e3) -> If (subst x v e1, subst x v e2, subst x v e3)

(** [step e] takes a single step of evaluation of [e]. *)
let rec step (e : expr) : expr =
  match e with
  | Int _ | Bool _ -> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
      step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
      Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) ->
      Binop (bop, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (e1, e2, e3) -> If (step e1, e2, e3)
  | Let (x, v1, e2) when is_value v1 ->
      subst x v1 e2
  | Let (x, e1, e2) ->
      Let (x, step e1, e2)
  | Var _ -> failwith unbound_var_err

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2]. Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 =
  match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

(** [eval e] fully evaluates [e] to a value [v]. *)
let rec eval (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval

(** [interp s] interprets [s] by lexing, parsing, evaluating,
    and converting the result to string. *)
let interp (s : string) : string =
  s |> parse |> eval |> string_of_val
