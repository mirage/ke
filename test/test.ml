let () = Printexc.record_backtrace true

module Make (Q : Ke.Sigs.R) = struct
  let to_list q = Q.fold (fun a x -> x :: a) [] q |> List.rev

  let test_0 =
    Alcotest.test_case "test-0" `Quick
    @@ fun () ->
    let q = Q.create Bigarray.Int in
    Alcotest.(check (list int)) "[            ]" (to_list q) [] ;
    Q.push q 1 ;
    Alcotest.(check (list int)) "[ 1          ]" (to_list q) [1] ;
    Q.push q 2 ;
    Alcotest.(check (list int)) "[ 1; 2       ]" (to_list q) [1; 2] ;
    Q.push q 3 ;
    Alcotest.(check (list int)) "[ 1; 2; 3    ]" (to_list q) [1; 2; 3] ;
    Q.push q 4 ;
    Alcotest.(check (list int)) "[ 1; 2; 3; 4 ]" (to_list q) [1; 2; 3; 4] ;
    let n = Q.pop_exn q in
    Alcotest.(check (list int)) "[    2; 3; 4 ]" (to_list q) [2; 3; 4] ;
    Alcotest.(check int) "1" n 1 ;
    let n = Q.pop_exn q in
    Alcotest.(check (list int)) "[       3; 4 ]" (to_list q) [3; 4] ;
    Alcotest.(check int) "2" n 2 ;
    let n = Q.pop_exn q in
    Alcotest.(check (list int)) "[          4 ]" (to_list q) [4] ;
    Alcotest.(check int) "3" n 3 ;
    let n = Q.pop_exn q in
    Alcotest.(check (list int)) "[            ]" (to_list q) [] ;
    Alcotest.(check int) "4" n 4 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q))

  let test_1 =
    Alcotest.test_case "test-1" `Quick
    @@ fun () ->
    let q = Q.create Bigarray.Int in
    Alcotest.(check (list int)) "empty" (to_list q) [] ;
    Q.push q 1 ;
    let n = Q.pop_exn q in
    Alcotest.(check int) "1" n 1 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q)) ;
    Q.push q 2 ;
    let n = Q.pop_exn q in
    Alcotest.(check int) "2" n 2 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q)) ;
    Alcotest.(check bool) "empty" (Q.is_empty q) true

  let test_2 =
    Alcotest.test_case "test-2" `Quick
    @@ fun () ->
    let q = Q.create Bigarray.Int in
    Q.push q 1 ;
    Alcotest.(check int) "[ 1 ]" (Q.peek_exn q) 1 ;
    Q.push q 2 ;
    Alcotest.(check int) "[ 1; 2 ]" (Q.peek_exn q) 1 ;
    Q.push q 3 ;
    Alcotest.(check int) "[ 1; 2; 3 ]" (Q.peek_exn q) 1 ;
    Alcotest.(check int) "peek" (Q.peek_exn q) 1 ;
    Alcotest.(check int) "pop"  (Q.pop_exn q)  1 ;
    Alcotest.(check int) "peek" (Q.peek_exn q) 2 ;
    Alcotest.(check int) "pop"  (Q.pop_exn q)  2 ;
    Alcotest.(check int) "peek" (Q.peek_exn q) 3 ;
    Alcotest.(check int) "pop"  (Q.pop_exn q)  3 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q)) ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q)) ;
    Alcotest.(check bool) "empty" (Q.is_empty q) true

  let test_3 =
    Alcotest.test_case "test-3" `Quick
    @@ fun () ->
    let q = Q.create Bigarray.Int in
    for i= 1 to 10 do Q.push q i done ;
    Q.clear q ;
    Alcotest.(check int) "length" (Q.length q) 0 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q)) ;
    Q.push q 42 ;
    Alcotest.(check int) "[ 42 ]" (Q.pop_exn q) 42

  let test_4 =
    Alcotest.test_case "test-4" `Quick
    @@ fun () ->
    let q1 = Q.create Bigarray.Int in
    for i = 1 to 10 do Q.push q1 i done ;
    let q2 = Q.copy q1 in
    Alcotest.(check (list int)) "[ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]"
      (to_list q1) [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] ;
    Alcotest.(check (list int)) "[ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]"
      (to_list q2) [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] ;
    Alcotest.(check int) "length" (Q.length q1) 10 ;
    Alcotest.(check int) "length" (Q.length q2) 10 ;
    for i = 1 to 10 do Alcotest.(check int) (string_of_int i) (Q.pop_exn q1) i done ;
    for i = 1 to 10 do Alcotest.(check int) (string_of_int i) (Q.pop_exn q2) i done

  let test_5 =
    Alcotest.test_case "test-5" `Quick
    @@ fun () ->
    let q = Q.create Bigarray.Int in
    Alcotest.(check bool) "is_empty" (Q.is_empty q) true ;
    for i = 1 to 10 do
      Q.push q i ;
      Alcotest.(check int) "length" (Q.length q) i ;
      Alcotest.(check bool) "is_not_empty" (not (Q.is_empty q)) true ;
    done ;
    for i = 10 downto 1 do
      Alcotest.(check int) "length" (Q.length q) i ;
      Alcotest.(check bool) "is_not_empty" (not (Q.is_empty q)) true ;
      ignore (Q.pop_exn q) ;
    done ;
    Alcotest.(check int) "length" (Q.length q) 0 ;
    Alcotest.(check bool) "is_empty" (Q.is_empty q) true

  let test_6 =
    Alcotest.test_case "test-6" `Quick
    @@ fun () ->
    let q = Q.create Bigarray.Int in
    for i = 1 to 10 do Q.push q i done ;
    let i = ref 1 in
    Q.iter (fun j -> Alcotest.(check int) "iter" !i j; incr i) q
end

module Test_rke = Make (Ke.Rke)

let () = Alcotest.run "ke"
    [ "rke", [ Test_rke.test_0
             ; Test_rke.test_1
             ; Test_rke.test_2
             ; Test_rke.test_3
             ; Test_rke.test_4
             ; Test_rke.test_5
             ; Test_rke.test_6 ] ]
