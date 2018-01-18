(*
 * TD criterias
 * Must cover all the labels
 *)


open Language;;
open Utils;;

let rec find_expected_labels program =
    match program with
    | Skip (l) -> LabelSet.singleton l
    | Assign(l, _, _) -> LabelSet.singleton l
    | Compose (c1, c2) ->
        let labels_c1 = find_expected_labels c1
        and labels_c2 = find_expected_labels c2
        in LabelSet.union labels_c1 labels_c2
    | If (l, _, c1, c2) ->
        let labels_c1 = find_expected_labels c1
        and labels_c2 = find_expected_labels c2
        in let collected_labels = LabelSet.union labels_c1 labels_c2
        in LabelSet.add l collected_labels
    | While (l, _, c) ->
        let collected_labels = find_expected_labels c
        in LabelSet.add l collected_labels
    ;;

let rec find_hit_labels state program =
    match program with
    | Skip (l) -> LabelSet.singleton l
    | Assign (l, var, aexp) ->
        let new_value = Aexp.eval state aexp
        in
            Hashtbl.replace state var new_value;
            LabelSet.singleton l
    | Compose (c1, c2) ->
        let labels_c1 = find_hit_labels state c1
        and labels_c2 = find_hit_labels state c2
        in LabelSet.union labels_c1 labels_c2
    | If (l, bexp, c1, c2) ->
        let collected_labels =
            match (Bexp.eval state bexp) with
            | true  -> find_hit_labels state c1
            | false -> find_hit_labels state c2
        in
        LabelSet.add l collected_labels
    | While (l, bexp, c) as while_c ->
        let collected_labels =
            match (Bexp.eval state bexp) with
            | true ->
                let labels_c = find_hit_labels state c
                and labels_while_c = find_hit_labels state while_c
                in LabelSet.union labels_c labels_while_c
            | false -> LabelSet.empty
        in LabelSet.add l collected_labels
    ;;


(*
 * Check if a list of states explores all the labels *)
let satisfies_td states program =
    let expected_labels = find_expected_labels program
    and hit_labels = make_union_labels find_hit_labels states program
    in LabelSet.equal expected_labels hit_labels
