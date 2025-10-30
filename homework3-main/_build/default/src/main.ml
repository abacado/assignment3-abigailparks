open Ast

(* Public error strings *)
let bop_err = "invalid binary operation"
let if_guard_err = "invalid if guard"
let unbound_var_err = "unbound variable"

let string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"

let is_value = function
  | Int _ | Bool _ -> true
  | _ -> false

(* Substitution for let semantics *)
let rec subst (e : expr) (v : expr) (x : string) : expr =
  match e with
  | Var y -> if x = y then v else e
  | Int _ | Bool _ -> e
  | Binop (b, e1, e2) -> Binop (b, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
      let e1' = subst e1 v x in
      if x = y then Let (y, e1', e2)
      else Let (y, e1', subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)

(* Binary operation step helper *)
and step_bop (b : bop) (e1 : expr) (e2 : expr) : expr =
  match b, e1, e2 with
  | Plus,  Int a, Int b -> Int (a + b)
  | Times, Int a, Int b -> Int (a * b)
  | Leq,   Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

(* One small-step evaluation *)
let rec step (e : expr) : expr =
  match e with
  | Int _ | Bool _ -> failwith "Does not step"
  | Var _ -> failwith unbound_var_err
  | Binop (b, e1, e2) ->
      if not (is_value e1) then Binop (b, step e1, e2)
      else if not (is_value e2) then Binop (b, e1, step e2)
      else step_bop b e1 e2
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith if_guard_err
  | If (e1, e2, e3) -> If (step e1, e2, e3)

let rec eval (e : expr) : expr =
  if is_value e then e else eval (step e)

let parse (s : string) : expr =
  let lb = Lexing.from_string s in
  Parser.main Lexer.read lb

let run (s : string) : string =
  string_of_val (eval (parse s))