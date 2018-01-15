(*
 * Language definition
 *)

type language = Skip    of string
              | Assign  of string   * Aexp.arith_expr
              | Compose of language * language
              | If      of string   * Bexp.bool_expr  * language * language
              | While   of string   * Bexp.bool_expr  * language
              ;;

let rec run state language =
    match language with
    | Skip    (label) ->
        Js.log {j|SKIP $label|j}
    | Assign  (label, expr) ->
        Js.log {j|ASSIGN $label|j};
        Hashtbl.replace state label ( Aexp.eval state expr )
    | Compose (c1, c2) ->
        run state c1;
        run state c2;
    | If      (label, cond, c1, c2) ->
        Js.log {j|IF $label|j};
        (
            match (Bexp.eval state cond) with
            | true  -> run state c1
            | false -> run state c2
        )
    | While   (label, cond, c) ->
        Js.log {j|WHILE $label|j};
        (
            match (Bexp.eval state cond) with
            | true -> run state c; run state (While(label, cond, c));
            | false -> ();
        )
