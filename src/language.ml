(*
 * Language definition
 *)

type language = Skip    of string
              | Assign  of string   * string          * Aexp.arith_expr
              | Compose of language * language
              | If      of string   * Bexp.bool_expr  * language * language
              | While   of string   * Bexp.bool_expr  * language
              ;;


module SS = Set.Make(String)

(*
 * Run a program
 * and collect labels hit during the execution
 *)

let rec run_and_collect_labels label_filter state program =
    match program with
    | Skip (l) as c ->
        label_filter c
    | Assign (l, var, aexp) as c ->
        Hashtbl.replace state var (Aexp.eval state aexp);
        label_filter c
    | Compose (c1, c2) ->
        let s1 = run_and_collect_labels label_filter state c1
        and s2 = run_and_collect_labels label_filter state c2
        in SS.union s1 s2
    | If (l, bexp, c1, c2) as c ->
        let s =
        match Bexp.eval state bexp with
        | true  -> run_and_collect_labels label_filter state c1
        | false -> run_and_collect_labels label_filter state c2
        in SS.union (label_filter c) s
    | While (l, bexp, c1) as c ->
        let s =
        match Bexp.eval state bexp with
        | true ->
            SS.union
                (run_and_collect_labels label_filter state c1)
                (run_and_collect_labels label_filter state c)
        | false -> SS.empty
        in SS.union (label_filter c) s


(*
 * Collect labels in the whole program
 *)
let rec collect_labels label_filter program =
    match program with
    | Skip (l) as c ->
        label_filter c
    | Assign (_, _, _) as c ->
        label_filter c
    | Compose (c1, c2) ->
        let s1 = collect_labels label_filter c1
        and s2 = collect_labels label_filter c2
        in SS.union s1 s2
    | If (l, _, c1, c2) as c ->
        let s1 = collect_labels label_filter c1
        and s2 = collect_labels label_filter c2
        and s3 = label_filter c
        in
        SS.union ( SS.union s1 s2) s3
    | While (l, _, c1) as c ->
        let s1 = collect_labels label_filter c1
        in SS.union s1 (label_filter c)

(*
 * Execute a program
 *)
let rec run =
    let dummy_label_filter c = SS.empty
    in run_and_collect_labels dummy_label_filter

let label_filter_none program =
    match program with
    | Skip (l) -> SS.singleton l
    | Assign(l, _, _) -> SS.singleton l
    | If (l, _, _, _) -> SS.singleton l
    | While (l, _, _) -> SS.singleton l
    ;;
