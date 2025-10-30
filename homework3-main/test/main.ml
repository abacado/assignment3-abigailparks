open OUnit2

module A = Calcpl.Ast
module M = Calcpl.Main

let make_i name expected input =
  name >:: fun _ ->
    match M.eval (M.parse input) with
    | A.Int i -> assert_equal expected i
    | _ -> assert_failure "expected Int"

let make_b name expected input =
  name >:: fun _ ->
    match M.eval (M.parse input) with
    | A.Bool b -> assert_equal expected b
    | _ -> assert_failure "expected Bool"

let make_t name err_str input =
  name >:: fun _ ->
    try
      let _ = M.eval (M.parse input) in
      assert_failure "expected failure"
    with Failure msg -> assert_equal err_str msg

let suite =
  "calcPL extended" >:::
  [
    make_b "true" true "true";
    make_b "false" false "false";
    make_b "leq" true "1<=1";
    make_b "leq1" false "2<=1";
    make_i "let" 22 "let x=22 in x";
    make_i "if1" 22 "if true then 22 else 0";

    make_t "invalid plus" M.bop_err "1 + true";
    make_t "invalid leq" M.bop_err "true <= 1";
    make_t "invalid guard" M.if_guard_err "if 1 then 2 else 3";
    make_t "unbound" M.unbound_var_err "x";

    make_i "int" 22 "22";
    make_i "add" 22 "11+11";
    make_i "adds" 22 "(10+1)+(5+6)";
    make_i "lets" 22 "let x = 0 in let x = 22 in x";
    make_i "mul1" 22 "2*11";
    make_i "mul2" 22 "2+2*10";
    make_i "mul3" 14 "2*2+10";
    make_i "mul4" 40 "2*2*10";
    make_b "leq again" true "1<=1";
    make_i "if2" 22 "if 1+2 <= 3+4 then 22 else 0";
    make_i "if3" 22 "if 1+2 <= 3*4 then let x = 22 in x else 0";
    make_i "letif" 22 "let x = 1+2 <= 3*4 in if x then 22 else 0";
    make_t "invalid mult" M.bop_err "1 * false";
  ]

let () = run_test_tt_main suite