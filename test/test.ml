let () = Printexc.record_backtrace true

module type Q = sig
  type t

  exception Empty

  val fold : ('a -> int -> 'a) -> 'a -> t -> 'a
  val create : unit -> t
  val push : t -> int -> unit
  val pop_exn : t -> int
  val peek_exn : t -> int
  val is_empty : t -> bool
  val length : t -> int
  val iter : (int -> unit) -> t -> unit
  val copy : t -> t
  val clear : t -> unit
end

module Make (Q : Q) = struct
  let to_list q = Q.fold (fun a x -> x :: a) [] q |> List.rev

  let test_0 =
    Alcotest.test_case "test-0" `Quick
    @@ fun () ->
    let q = Q.create () in
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
    let q = Q.create () in
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
    let q = Q.create () in
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
    let q = Q.create () in
    for i= 1 to 10 do Q.push q i done ;
    Q.clear q ;
    Alcotest.(check int) "length" (Q.length q) 0 ;
    Alcotest.check_raises "exception" Q.Empty (fun () -> ignore (Q.pop_exn q)) ;
    Q.push q 42 ;
    Alcotest.(check int) "[ 42 ]" (Q.pop_exn q) 42

  let test_4 =
    Alcotest.test_case "test-4" `Quick
    @@ fun () ->
    let q1 = Q.create () in
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
    let q = Q.create () in
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
    let q = Q.create () in
    for i = 1 to 10 do Q.push q i done ;
    let i = ref 1 in
    Q.iter (fun j -> Alcotest.(check int) "iter" !i j; incr i) q
end

module Test_rke = Make (struct
    type t = (int, Bigarray.int_elt) Ke.Rke.t

    module W : sig exception Empty end = struct include Ke.Rke end
    include W

    let fold = Ke.Rke.fold
    let create () = Ke.Rke.create Bigarray.Int
    let push = Ke.Rke.push
    let pop_exn = Ke.Rke.pop_exn
    let peek_exn = Ke.Rke.peek_exn
    let is_empty = Ke.Rke.is_empty
    let length = Ke.Rke.length
    let iter = Ke.Rke.iter
    let copy = Ke.Rke.copy
    let clear = Ke.Rke.clear
  end)

module Test_weighted_rke = Make (struct
    type t = (int, Bigarray.int_elt) Ke.Rke.Weighted.t

    module W : sig exception Empty end = struct include Ke.Rke.Weighted end
    include W

    let fold = Ke.Rke.Weighted.fold
    let create () = let q, _ = Ke.Rke.Weighted.create ~capacity:0x100 Bigarray.Int in q
    let push = Ke.Rke.Weighted.push_exn
    let pop_exn = Ke.Rke.Weighted.pop_exn
    let peek_exn = Ke.Rke.Weighted.peek_exn
    let is_empty = Ke.Rke.Weighted.is_empty
    let length = Ke.Rke.Weighted.length
    let iter = Ke.Rke.Weighted.iter
    let copy = Ke.Rke.Weighted.copy
    let clear = Ke.Rke.Weighted.clear
  end)

let () = Alcotest.run "ke"
    [ "rke", [ Test_rke.test_0
             ; Test_rke.test_1
             ; Test_rke.test_2
             ; Test_rke.test_3
             ; Test_rke.test_4
             ; Test_rke.test_5
             ; Test_rke.test_6 ]
    ; "rke:weighted", [ Test_weighted_rke.test_0
                      ; Test_weighted_rke.test_1
                      ; Test_weighted_rke.test_2
                      ; Test_weighted_rke.test_3
                      ; Test_weighted_rke.test_4
                      ; Test_weighted_rke.test_5
                      ; Test_weighted_rke.test_6 ] ]
