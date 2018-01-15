open Jest;;

let _ =

describe "Comparisons are correct" (fun () ->
  let open Expect in
    let expect_eval_expr_equals expr value =
      let state = Hashtbl.create 1 in
      Hashtbl.replace state "a" 10;
      expect (Bexp.eval state expr) |> toBe value
    in

    test "1 = 1 is true" (fun () ->
        let expr  = Bexp.Eq(Aexp.Const(1), Aexp.Const(1))
        in expect_eval_expr_equals expr true
    );

    test "2 = 1 is false" (fun () ->
        let expr  = Bexp.Eq(Aexp.Const(2), Aexp.Const(1))
        in expect_eval_expr_equals expr false
    );

    test "1 >= 1 is true" (fun () ->
        let expr = Bexp.Ge(Aexp.Const(1), Aexp.Const(1))
        in expect_eval_expr_equals expr true
    );

    test "2 >= 1 is true" (fun () ->
        let expr = Bexp.Ge(Aexp.Const(2), Aexp.Const(1))
        in expect_eval_expr_equals expr true
    );

    test "1 >= 2 is false" (fun () ->
        let expr = Bexp.Ge(Aexp.Const(1), Aexp.Const(2))
        in expect_eval_expr_equals expr false
    );

    test "2 > 1 is true" (fun () ->
        let expr = Bexp.Gt(Aexp.Const(2), Aexp.Const(1))
        in expect_eval_expr_equals expr true
    );

    test "1 > 1 is false" (fun () ->
        let expr = Bexp.Gt(Aexp.Const(1), Aexp.Const(1))
        in expect_eval_expr_equals expr false
    );

    test "1 > 2 is false" (fun () ->
        let expr = Bexp.Gt(Aexp.Const(1), Aexp.Const(2))
        in expect_eval_expr_equals expr false
    );

    test "1 <= 2 is true" (fun () ->
        let expr = Bexp.Le(Aexp.Const(1), Aexp.Const(2))
        in expect_eval_expr_equals expr true
    );

    test "1 <= 1 is true" (fun () ->
        let expr = Bexp.Le(Aexp.Const(1), Aexp.Const(1))
        in expect_eval_expr_equals expr true
    );

    test "2 <= 1 is false" (fun () ->
        let expr = Bexp.Le(Aexp.Const(2), Aexp.Const(1))
        in expect_eval_expr_equals expr false
    );
);
