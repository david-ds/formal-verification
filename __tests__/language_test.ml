open Jest;;

let _ =

describe "Operations are correct" (fun () ->
  let open Expect in

    let expect_state_value state variable value =
        expect (Hashtbl.find state variable) |> toBe value
    in

    test "assign operation is correct" (fun () ->
        let language = Language.Assign("a", Aexp.Const(2))
        and state    = Hashtbl.create 1
        in
        Language.run state language;
        expect_state_value state "a" 2
    );

    test "incrementation is correct" (fun () ->
        let language = Language.Assign("a", Aexp.Plus(Aexp.Var("a"), Aexp.Const(1)))
        and state = Hashtbl.create 1
        in
        Hashtbl.replace state "a" 3;
        Language.run state language;
        expect_state_value state "a" 4
    );

    test "condition is correct" (fun () ->
        let language = Language.If(
            "label_if",
            Bexp.Const(true),
            Language.Assign("a", Aexp.Const(3)),
            Language.Assign("a", Aexp.Const(-1))
        )
        and state = Hashtbl.create 1
        in
        Language.run state language;
        expect_state_value state "a" 3
    );

    test "loop is correct" (fun () ->
        let language = Language.While(
            "label_while",
            Bexp.Lt(Aexp.Var("a"), Aexp.Const(5)),
            Language.Assign("a", Aexp.Plus(Aexp.Var("a"), Aexp.Const(1)))
        )
        and state = Hashtbl.create 1
        in
        Hashtbl.replace state "a" 1;
        Language.run state language;
        expect_state_value state "a" 5
    );

);
