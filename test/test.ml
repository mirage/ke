let () = Printexc.record_backtrace true

module Make (Q : Ke.Sigs.M) = struct

  let to_list q =
    Q.fold (fun a x -> x :: a) [] q |> List.rev

  let test_0 =
    Alcotest.test_case "test-0" `Quick @@ fun () ->
    let q = Q.create () in Alcotest.(check (list int)) "[            ]" (to_list q) [            ] ;
    Q.push q 1;            Alcotest.(check (list int)) "[ 1          ]" (to_list q) [ 1          ] ;
    Q.push q 2;            Alcotest.(check (list int)) "[ 1; 2       ]" (to_list q) [ 1; 2       ] ;
    Q.push q 3;            Alcotest.(check (list int)) "[ 1; 2; 3    ]" (to_list q) [ 1; 2; 3    ] ;
    Q.push q 4;            Alcotest.(check (list int)) "[ 1; 2; 3; 4 ]" (to_list q) [ 1; 2; 3; 4 ] ;
    let n = Q.pop_exn q in Alcotest.(check (list int)) "[    2; 3; 4 ]" (to_list q) [    2; 3; 4 ] ;
                           Alcotest.(check int) "1" n 1 ;
    let n = Q.pop_exn q in Alcotest.(check (list int)) "[       3; 4 ]" (to_list q) [       3; 4 ] ;
                           Alcotest.(check int) "2" n 2 ;
    let n = Q.pop_exn q in Alcotest.(check (list int)) "[          4 ]" (to_list q) [          4 ] ;
                           Alcotest.(check int) "3" n 3 ;
    let n = Q.pop_exn q in Alcotest.(check (list int)) "[            ]" (to_list q) [            ] ;
                           Alcotest.(check int) "4" n 4 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q))

  let test_1 =
    Alcotest.test_case "test-1" `Quick @@ fun () ->
    let q = Q.create () in Alcotest.(check (list int)) "empty" (to_list q) [] ;
    Q.push q 1;
    let n = Q.pop_exn q in Alcotest.(check int) "1" n 1 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q)) ;
    Q.push q 2;
    let n = Q.pop_exn q in Alcotest.(check int) "2" n 2 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q)) ;
    Alcotest.(check bool) "empty" (Q.is_empty q) true
end

module Test_mke = Make(Ke.Mke)

let () =
  Alcotest.run "ke" [ "mke", [ Test_mke.test_0
                             ; Test_mke.test_1 ] ]
