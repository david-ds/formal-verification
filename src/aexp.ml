(*
 * Arithmetic expressions
 *
 *)

type arith_expr = Const of int
                | Var   of string
                | Plus  of arith_expr * arith_expr
                | Minus of arith_expr * arith_expr
                | Mult  of arith_expr * arith_expr
                | Div   of arith_expr * arith_expr
                ;;

let eval state arith_expr =

    let rec eval_aux arith_expr =
        match arith_expr with
        | Const (value) -> value
        | Var   (value) -> Hashtbl.find state value

        | Plus  (expr1, expr2) -> eval_aux(expr1) + eval_aux(expr2)
        | Minus (expr1, expr2) -> eval_aux(expr1) - eval_aux(expr2)
        | Mult  (expr1, expr2) -> eval_aux(expr1) * eval_aux(expr2)
        | Div   (expr1, expr2) -> eval_aux(expr1) / eval_aux(expr2)

    in eval_aux arith_expr
