(*
 * Critere TA aka "Toutes les Affectations"
 *
 * All the affectations are hit
 *)

module LabelSet = Set.Make(String)

open Language;;

(*
 * Find all labels corresponding to an ASSIGN node
 *)
let rec find_expected_labels program =
    match program with
    | Skip (_) -> LabelSet.empty
    | Assign (l, _, _) -> LabelSet.singleton l
    | Compose (c1, c2) ->
        let labels_c1 = find_expected_labels c1
        and labels_c2 = find_expected_labels c2
        in LabelSet.union labels_c1 labels_c2
    | If (_, _, c1, c2) ->
        let labels_c1 = find_expected_labels c1
        and labels_c2 = find_expected_labels c2
        in LabelSet.union labels_c1 labels_c2
    | While(_, _, c) -> find_expected_labels c

(*
 * Find hit ASSIGN labels during the execution
 *)
let rec find_hit_labels state program =
    match program with
    | Skip (_) -> LabelSet.empty
    | Assign (l, var, aexp) ->
        let new_value = Aexp.eval state aexp
        in
            Hashtbl.replace state var new_value;
            LabelSet.singleton l
    | Compose (c1, c2) ->
        let labels_c1 = find_hit_labels state c1
        and labels_c2 = find_hit_labels state c2
        in LabelSet.union labels_c1 labels_c2
    | If (_, bexp, c1, c2) ->
        (
            match (Bexp.eval state bexp) with
            | true  -> find_hit_labels state c1
            | false -> find_hit_labels state c2
        )

    | While (_, bexp, c) as while_c ->
        (
            match (Bexp.eval state bexp) with
            | true  ->
                let labels_c = find_hit_labels state c
                and labels_while_c = find_hit_labels state while_c
                in LabelSet.union labels_c labels_while_c
            | false -> LabelSet.empty
        );;

(*
 * Build the union of the collected labels
 *)
let rec make_union_labels states program =
    match states with
    | [] -> LabelSet.empty
    | state::q ->
        let collected_labels = find_hit_labels state program
        in LabelSet.union collected_labels (make_union_labels q program)

(*
 * Check if a list of states match all the ASSIGN statements *)
let satisfies_ta states program =
    let expected_labels = find_expected_labels program
    and hit_labels = make_union_labels states program
    in LabelSet.equal expected_labels hit_labels
