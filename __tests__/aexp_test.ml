open Jest;;

let _ =

describe "Operations without state" (fun () ->
  let open Expect in
    let expect_eval_expr_equals expr value =
      let state = Hashtbl.create 0 in
      expect (Aexp.eval state expr) |> toBe value
    in

    test "evaluate addition should return the correct result" (fun () ->
      let expr = Aexp.Plus(Aexp.Const(1), Aexp.Const(2)) in
      expect_eval_expr_equals expr 3
    );

    test "evaluate substraction should return the correct result" (fun () ->
      let expr = Aexp.Minus(Aexp.Const(3), Aexp.Const(1)) in
      expect_eval_expr_equals expr 2
    );

    test "evaluate multiplication should return the correct result" (fun () ->
      let expr = Aexp.Mult(Aexp.Const(3), Aexp.Const(2)) in
      expect_eval_expr_equals expr 6
    );

    test "evaluate division should return the correct result" (fun () ->
      let expr = Aexp.Div(Aexp.Const(6), Aexp.Const(2)) in
      expect_eval_expr_equals expr 3
    );

    test "evaluate complex expression should return the correct result" (fun () ->
      let expr = Aexp.Plus(Aexp.Mult(Aexp.Const(5), Aexp.Const(2)), Aexp.Const(3)) in
      expect_eval_expr_equals expr 13
    );
);

describe "Operations with state" (fun () ->
  let open Expect in
    let expect_eval_expr_equals state expr value =
      expect (Aexp.eval state expr) |> toBe value
    in

    test "substitution of variable works" (fun () ->
      let state = Hashtbl.create 1 in
      let expr = Aexp.Var("a") in
      Hashtbl.replace state "a" 10;
      expect_eval_expr_equals state expr 10
    );

    test "addition of two variables works" (fun () ->
      let state = Hashtbl.create 2 in
      let expr = Aexp.Plus(Aexp.Var("a"), Aexp.Var("b")) in
      Hashtbl.replace state "a" 3;
      Hashtbl.replace state "b" 5;
      expect_eval_expr_equals state expr 8
    );
);
