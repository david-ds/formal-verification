(*
 * Language definition
 *)

type language = Skip    of string
              | Assign  of string   * string          * Aexp.arith_expr
              | Compose of language * language
              | If      of string   * Bexp.bool_expr  * language * language
              | While   of string   * Bexp.bool_expr  * language
              ;;


(*
 * Execute a program
 *)
let rec execute state program =
    match program with
    | Skip (l) -> ()
    | Assign (l, var, aexp) ->
        Hashtbl.replace state var (Aexp.eval state aexp)
    | Compose (c1, c2) ->
        execute state c1;
        execute state c2
    | If (l, bexp, c1, c2) ->
        (
            match (Bexp.eval state bexp) with
            | true  -> execute state c1
            | false -> execute state c2
        )
    | While (l, bexp, c) as while_c ->
        (
            match (Bexp.eval state bexp) with
            | true  ->
                execute state c;
                execute state while_c
            | false -> ()
        )
    ;;

