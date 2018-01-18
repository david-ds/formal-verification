open Jest;;

let _ =

describe "prog is correct" (fun () ->
  let open Expect in

    let expect_result x value =
        let state = Hashtbl.create 1
        in
        Hashtbl.replace state "X" x;
        Language.execute state Main.prog;
        expect (Hashtbl.find state "X") |> toBe value
    in

    test "prog(0) = 1"  (fun () -> expect_result  0   1);

    test "prog(-1) = 1" (fun () -> expect_result (-1) 1);

    test "prog(1) = 1"  (fun () -> expect_result  1   1);

    test "prog(2) = 0"  (fun () -> expect_result  2   0);
);

describe "prog_modulo is correct" (fun () ->
  let open Expect in

    let expect_result x y expected_modulo =
        let state = Hashtbl.create 2
        in
        Hashtbl.replace state "X" x;
        Hashtbl.replace state "Y" y;
        Language.execute state Main.prog_modulo;
        expect (Hashtbl.find state "X") |> toBe expected_modulo
    in

    test "prog_modulo(100, 10) = 0"  (fun () -> expect_result  100 10 0);

    test "prog_modulo(10, 3) = 1"    (fun () -> expect_result   10  3 1);

    test "prog_modulo(-20, 7) = 6"          (fun () -> expect_result (-20) 7 6);
);
