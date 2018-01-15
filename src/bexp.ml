(*
 * Boolean expressions
 *)

type bool_expr = Const of bool
               | Eq of Aexp.arith_expr * Aexp.arith_expr
               | Ge of Aexp.arith_expr * Aexp.arith_expr
               | Gt of Aexp.arith_expr * Aexp.arith_expr
               | Le of Aexp.arith_expr * Aexp.arith_expr
               | Lt of Aexp.arith_expr * Aexp.arith_expr
              ;;

let eval state bool_expr =

    let eval_aux bool_expr =
        let eval_aexpr expr = Aexp.eval state expr
        in
        match bool_expr with
        | Const (value) -> value

        | Eq (expr1, expr2) -> eval_aexpr expr1 == eval_aexpr expr2
        | Ge (expr1, expr2) -> eval_aexpr expr1 >= eval_aexpr expr2
        | Gt (expr1, expr2) -> eval_aexpr expr1 >  eval_aexpr expr2
        | Le (expr1, expr2) -> eval_aexpr expr1 <= eval_aexpr expr2
        | Lt (expr1, expr2) -> eval_aexpr expr1 <  eval_aexpr expr2

    in eval_aux bool_expr
