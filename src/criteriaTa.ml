(*
 * Critere TA aka "Toutes les Affectations"
 *
 * Tous les labels apparaissent au moins une fois
 * dans les données de test
 *)

let init_state var init_value =
    let state = Hashtbl.create 1 in
    Hashtbl.replace state var init_value;
    state

(* run the program for each state and build the union of the labels *)
let rec build_labels_union states program =
    match states with
    | [] -> Language.SS.empty
    | state::q -> Language.SS.union ( Language.run_and_collect_labels state program ) ( build_labels_union q program )


let is_ta_criteria states program =
    let union_labels = build_labels_union states program
    and all_labels = Language.collect_labels program
    in Language.SS.equal all_labels union_labels
