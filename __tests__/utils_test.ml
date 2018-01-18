open Jest;;

open Utils;;

let _ =

describe "Build union of labels" (fun () ->
  let open Expect in
    let init_state var init_value =
        let state = Hashtbl.create 1
        in Hashtbl.replace state var init_value;
        state
    in


    test "run prog with one data test"  (fun () ->
			let states = [
					( init_state "X"  1 );   (* 1 -> *3* -> 4 -> *6* *)
			]
			and expected_labels = LabelSet.of_list ["3";"6"]
			in let labels = make_union_labels CriteriaTa.find_hit_labels states (Main.prog)
			in expect (LabelSet.equal labels expected_labels) |> toBe true
    );

    test "run program with two data sets" (fun () ->
			let states = [
					( init_state "X"  1 );   (* 1 -> *3* -> 4 -> *6* *)
					( init_state "X" (-1) )  (* 1 -> *2* -> 4 -> *5* *)
			]
			and expected_labels = LabelSet.of_list ["2"; "3"; "5"; "6"]

			in let labels = make_union_labels CriteriaTa.find_hit_labels states (Main.prog)
			in expect (LabelSet.equal labels expected_labels) |> toBe true
    );
);
