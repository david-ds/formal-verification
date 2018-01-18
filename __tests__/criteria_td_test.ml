open Jest;;

let _ =



describe "Determine if a data set for prog is a TD criteria" (fun () ->
	let open Expect in
		let init_state var init_value =
			let state = Hashtbl.create 1
			in Hashtbl.replace state var init_value;
			state
		in

		test "a TD data set should be recognized" (fun () ->
			let states = [
					( init_state "X"  1 );   (* 1 -> 3 -> 4 -> 6 *)
					( init_state "X" (-1) )  (* 1 -> 2 -> 4 -> 5 *)
			]
			in expect (CriteriaTd.satisfies_td states Main.prog) |> toBe true
		);

		test "A non-TD data set should not be recongnized" (fun () ->
			let states = [
				( init_state "X" 1 ); (* 1 -> 3 -> 4 -> 6 *)
				( init_state "X" 0 )  (* 1 -> 2 -> 4 -> 6 *)
			]
			in expect (CriteriaTd.satisfies_td states Main.prog) |> toBe false
		);
);

describe "Determine if a data set for prog_modulo is a TD criteria" (fun () ->
	let open Expect in
		let init_state valX valY =
			let state = Hashtbl.create 2
			in Hashtbl.replace state "X" valX;
			Hashtbl.replace state "Y" valY;
			state
		in

        test "A TD data set should not be recongnized" (fun () ->
			let states = [
				( init_state (-11) 10 ); (* 1 -> 2 -> 4 -> 5 -> 4 *)
				( init_state 6 3 )       (* 1 -> 3 -> 4 -> 5 -> 4 -> 5 -> 4 *)
			]
			in expect (CriteriaTd.satisfies_td states Main.prog_modulo) |> toBe true
		);

		test "a non-TD data set should be recognized" (fun () ->
			let states = [
					( init_state 10 3 );   (* 1 -> 3 -> 4 -> 5 -> 4 -> 5 -> 4 -> 5 -> 4 *)
					( init_state 10 6 )    (* 1 -> 3 -> 4 -> 5 -> 4 *)
			]
			in expect (CriteriaTd.satisfies_td states Main.prog_modulo) |> toBe false
		);
);

