(*
 * Critere TA aka "Toutes les Affectations"
 *
 * All the affectations are achieved
 *)

(*
 * Label Filter : filter only Assign labels
 *)
let label_filter program =
    match program with
    | Language.Assign (l, _, _) -> Language.SS.singleton l
    | _                -> Language.SS.empty
    ;;


(*
 * Browse a program multiple times with different states
 * and return the collected labels
 *)
let make_union_labels = Criteria.make_union_labels label_filter

let satisfies_ta states program =
    (* all ASSIGN labels *)
    let expected_labels = Language.collect_labels label_filter program
    (* union of all hit ASSIGN labels *)
    and labels = make_union_labels states program
    in Language.SS.equal expected_labels labels
