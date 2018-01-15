open Jest;;

let _ =

describe "Program is correct" (fun () ->
  let open Expect in

    let expect_result x value =
        let state = Hashtbl.create 1
        in
        Hashtbl.replace state "X" x;
        Language.run state Main.prog;
        expect (Hashtbl.find state "X") |> toBe value
    in

    test "prog(0) = 1"  (fun () -> expect_result  0   1);

    test "prog(-1) = 1" (fun () -> expect_result (-1) 1);

    test "prog(1) = 1"  (fun () -> expect_result  1   1);

    test "prog(2) = 0"  (fun () -> expect_result  2   0);
);