(*
 * Util generic functions for criterias
 *)

(*
 * Run a program with multiple data states
 * and build the union of the results
 *)
let rec make_union_labels label_filter states program =
    match states with
    | [] -> Language.SS.empty
    | state::q ->
        let s1 = Language.run_and_collect_labels label_filter state program
        and s2 = make_union_labels label_filter q program
        in Language.SS.union s1 s2
    ;;
