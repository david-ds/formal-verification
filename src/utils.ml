(*
 * Utils function used in multiple criterias
 *)


module LabelSet = Set.Make(String)

(*
 * Build the union of the collected labels
 *)
let rec make_union_labels f states program =
    match states with
    | [] -> LabelSet.empty
    | state::q ->
        let collected_labels = f state program
        in LabelSet.union collected_labels (make_union_labels f q program)
