open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let unbound_var_err = "Unbound variable"
let bop_err = "Operator and operand type mismatch"
let if_guard_err = "Guard of if must have type bool"

let string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"

let is_value = function
  | Int _ | Bool _ -> true
  | Binop _ -> false

let rec step = function
  | Int _ | Bool _ -> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
      step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
      Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) ->
      Binop (bop, step e1, e2)

and step_bop bop e1 e2 =
  match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | _ -> failwith "precondition violated"

let rec eval e =
  if is_value e then e else e |> step |> eval

let interp (s : string) : string =
  s |> parse |> eval |> string_of_val

let () =
  try
    let input = read_line () in
    let result = interp input in
    print_endline result
  with
  | Failure msg -> print_endline ("Error: " ^ msg)
  | _ -> print_endline "Unexpected error"
