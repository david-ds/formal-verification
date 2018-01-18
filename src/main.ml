(*
 * main file
 *)

let prog = Language.Compose(

    (* if X <= 0 then X := -X else X := 1 - X *)

    Language.If("1",
        Bexp.Le(Aexp.Var("X"), Aexp.Const(0)),
        Language.Assign("2", "X", Aexp.Minus(Aexp.Const(0), Aexp.Var("X"))),
        Language.Assign("3", "X", Aexp.Minus(Aexp.Const(1), Aexp.Var("X")))
    ),

    (* if X = 1 then X := 1 else X := X + 1 *)

    Language.If("4",
        Bexp.Eq(Aexp.Var("X"), Aexp.Const(1)),
        Language.Assign("5", "X", Aexp.Const(1)),
        Language.Assign("6", "X", Aexp.Plus(Aexp.Var("X"), Aexp.Const(1)))
    )
);;


(*
 * Modulo program
 * Computes X mod Y
 * Result is in X
 *)
let prog_modulo = Language.Compose(

    (* if X <= 0 then X := -X *)

    Language.If("1",
        Bexp.Le(Aexp.Var("X"), Aexp.Const(0)),
        Language.Assign("2", "X", Aexp.Minus(Aexp.Const(0), Aexp.Var("X"))),
        Language.Skip("3")
    ),

    (* while X >= Y do X := X-Y *)

    Language.While("4",
        Bexp.Ge(Aexp.Var("X"), Aexp.Var("Y")),
        Language.Assign("5", "X", Aexp.Minus(Aexp.Var("X"), Aexp.Var("Y")))
    )
);;
