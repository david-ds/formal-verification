(*
 * Language definition
 *)

module SS = Set.Make(String)

type language = Skip    of string
              | Assign  of string   * string          * Aexp.arith_expr
              | Compose of language * language
              | If      of string   * Bexp.bool_expr  * language * language
              | While   of string   * Bexp.bool_expr  * language
              ;;

(*
 * Find labels of a program for a given execution
 * identified by an initial state
 *)
let rec run_and_collect_labels state language =
    match language with
    | Skip (label) -> SS.singleton label
    | Assign (label, variable, expr) ->
        Hashtbl.replace state variable ( Aexp.eval state expr );
        SS.singleton label;
    | Compose (c1, c2) ->
        SS.union (run_and_collect_labels state c1) (run_and_collect_labels state c2)
    | If (label, cond, c1, c2) ->
        let labels =
            match ( Bexp.eval state cond) with
            | true  -> run_and_collect_labels state c1
            | false -> run_and_collect_labels state c2
        in SS.add label labels
    | While (label, cond, c) ->
        let new_labels cond c =
            match ( Bexp.eval state cond) with
            | true ->
                SS.union (run_and_collect_labels state c) (run_and_collect_labels state (While(label, cond, c)))
            | false ->
                SS.empty
        in SS.add label (new_labels cond c)

let run state language =
    run_and_collect_labels state language;
    ();;

(*
 * Find all the labels of a program
 *)
let rec collect_labels language =
    match language with
    | Skip    (label)            -> SS.singleton label
    | Assign  (label, _, _)      -> SS.singleton label
    | Compose (c1, c2)           ->
        let s1 = collect_labels c1
        and s2 = collect_labels c2
        in  SS.union s1 s2
    | If      (label, _, c1, c2) ->
        let s1 = collect_labels c1
        and s2 = collect_labels c2
        in let s = SS.union s1 s2
        in SS.add label s
    | While   (label, _, c)      ->
        let s = collect_labels c
        in SS.add label s
    ;;
