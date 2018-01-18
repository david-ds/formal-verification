open Jest;;

let _ =

describe "Build union of labels" (fun () ->
  let open Expect in
    let init_state var init_value =
        let state = Hashtbl.create 1
        in Hashtbl.replace state var init_value;
        state
    in


    test "run program once"  (fun () ->
			let states = [
					( init_state "X"  1 );   (* 1 -> *3* -> 4 -> *6* *)
			]
			and expected_labels = Language.SS.of_list ["3";"6"]
			in
			let labels = CriteriaTa.make_union_labels states (Main.prog)
			in
			expect (Language.SS.equal labels expected_labels) |> toBe true
    );

    test "run program twice" (fun () ->
			let states = [
					( init_state "X"  1 );   (* 1 -> *3* -> 4 -> *6* *)
					( init_state "X" (-1) )  (* 1 -> *2* -> 4 -> *5* *)
			]
			and expected_labels = Language.SS.of_list ["2"; "3"; "5"; "6"]

			in let labels = CriteriaTa.make_union_labels states (Main.prog)
			in expect (Language.SS.equal labels expected_labels) |> toBe true
    );
);


describe "Determine if a data set is a TA criteria" (fun () ->
	let open Expect in
		let init_state var init_value =
			let state = Hashtbl.create 1
			in Hashtbl.replace state var init_value;
			state
		in

		test "a TA data set should be recognized" (fun () ->
			let states = [
					( init_state "X"  1 );   (* 1 -> *3* -> 4 -> *6* *)
					( init_state "X" (-1) )  (* 1 -> *2* -> 4 -> *5* *)
			]
			in expect (CriteriaTa.satisfies_ta states Main.prog) |> toBe true
		);

		test "A no-TA data set should not be recongnized" (fun () ->
			let states = [
				( init_state "X" 3 ); (* 1 -> *3* -> 4 -> *5* *)
				( init_state "X" 4 )  (* 1 -> *3* -> 4 -> *5* *)
			]
			in expect (CriteriaTa.satisfies_ta states Main.prog) |> toBe false
		);
);
