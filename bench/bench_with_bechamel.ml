open Bechamel
open Toolkit

let random ln =
  let ic = open_in "/dev/urandom" in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ; close_in ic ; Bytes.unsafe_to_string rs

let push_fke n =
  let raw = random n in
  let data = List.init n (String.get raw) in
  Staged.stage (fun () -> List.fold_left Ke.Fke.push Ke.Fke.empty data)

let push_rke n =
  let queue = Ke.Rke.create ~capacity:n Bigarray.Char in
  let raw = random n in
  Staged.stage (fun () -> String.iter (Ke.Rke.push queue) raw)

let push_rke_n n =
  let queue = Ke.Rke.create ~capacity:n Bigarray.Char in
  let raw = random n in
  let blit src src_off dst dst_off len =
    Bigstringaf.unsafe_blit_from_string src ~src_off dst ~dst_off ~len
  in
  Staged.stage (fun () -> Ke.Rke.N.push queue ~blit ~length:String.length raw)

let push_queue n =
  let queue = Queue.create () in
  let raw = random n in
  Staged.stage (fun () -> String.iter (fun chr -> Queue.add chr queue) raw)

let push_and_pop_fke n =
  let raw = random n in
  let data = List.init n (String.get raw) in
  Staged.stage (fun () ->
      let q = List.fold_left Ke.Fke.push Ke.Fke.empty data in
      let rec go q =
        if not (Ke.Fke.is_empty q) then
          let _, q = Ke.Fke.pop_exn q in
          go q
        else ()
      in
      go q )

let push_and_pop_rke n =
  let queue = Ke.Rke.create ~capacity:n Bigarray.Char in
  let raw = random n in
  Staged.stage (fun () ->
      String.iter (Ke.Rke.push queue) raw ;
      while not (Ke.Rke.is_empty queue) do
        ignore (Ke.Rke.pop queue)
      done )

let push_and_pop_queue n =
  let queue = Queue.create () in
  let raw = random n in
  Staged.stage (fun () ->
      String.iter (fun chr -> Queue.add chr queue) raw ;
      while not (Queue.is_empty queue) do
        ignore (Queue.pop queue)
      done )

let test_push_fke =
  Test.make_indexed ~name:"Fke.push"
    ~args:[100; 500; 1000; 5000; 10000]
    push_fke

let test_push_rke =
  Test.make_indexed ~name:"Rke.push"
    ~args:[100; 500; 1000; 5000; 10000]
    push_rke

let test_push_rke_n =
  Test.make_indexed ~name:"Rke.N.push"
    ~args:[100; 500; 1000; 5000; 10000]
    push_rke_n

let test_push_queue =
  Test.make_indexed ~name:"Queue.push"
    ~args:[100; 500; 1000; 5000; 10000]
    push_queue

let tests_push =
  [test_push_fke; test_push_rke; test_push_rke_n; test_push_queue]

let big_push_fke n =
  Staged.stage
  @@ fun () ->
  let q = ref Ke.Fke.empty in
  for i = 1 to n do
    q := Ke.Fke.push !q i
  done

let big_push_rke n =
  Staged.stage
  @@ fun () ->
  let q = Ke.Rke.create ~capacity:n Bigarray.Char in
  for i = 1 to n do
    Ke.Rke.push q (Obj.magic i)
  done

let big_push_queue n =
  Staged.stage
  @@ fun () ->
  let q = Queue.create () in
  for i = 1 to n do
    Queue.push i q
  done

let test_big_push_fke =
  Test.make_indexed ~name:"Fke.big_push" ~args:[10; 1_000_000] big_push_fke

let test_big_push_rke =
  Test.make_indexed ~name:"Rke.big_push" ~args:[10; 1_000_000] big_push_rke

let test_big_push_queue =
  Test.make_indexed ~name:"Queue.big_push" ~args:[10; 1_000_000] big_push_queue

let tests_big_push = [test_big_push_fke; test_big_push_rke; test_big_push_queue]

let test_push_and_pop_fke =
  Test.make_indexed ~name:"Fke.push & Fke.pop"
    ~args:[100; 500; 1000; 5000; 10000]
    push_and_pop_fke

let test_push_and_pop_rke =
  Test.make_indexed ~name:"Rke.push & Rke.pop"
    ~args:[100; 500; 1000; 5000; 10000]
    push_and_pop_rke

let test_push_and_pop_queue =
  Test.make_indexed ~name:"Queue.push & Queue.pop"
    ~args:[100; 500; 1000; 5000; 10000]
    push_and_pop_queue

let tests_push_and_pop =
  [test_push_and_pop_fke; test_push_and_pop_rke; test_push_and_pop_queue]

let () = Bechamel_notty.Unit.add Instance.monotonic_clock "ns"
let () = Bechamel_notty.Unit.add Instance.minor_allocated "w"
let () = Bechamel_notty.Unit.add Instance.major_allocated "mw"
let () = Bechamel_notty.Unit.add Bechamel_perf.Instance.cpu_clock "ns"

let (<.>) f g = fun x -> f (g x)

let () =
  let ols = Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:Measure.[|run|] in
  let instances = Instance.[ minor_allocated
                           ; major_allocated
                           ; Bechamel_perf.Instance.cpu_clock ]
  in
  let tests =
    match Sys.argv with
    | [|_|] -> []
    | [|_; "push"|] -> tests_push
    | [|_; "push&pop"|] -> tests_push_and_pop
    | [|_; "big-push"|] -> tests_big_push
    | [|_; "all"|] -> tests_push @ tests_big_push @ tests_push_and_pop
    | _ -> Fmt.invalid_arg "%s {push|all}" Sys.argv.(1)
  in
  let raw_results = List.map (Benchmark.all ~run:3000 ~quota:Benchmark.(s 1.5) instances) tests in
  let results = List.map (fun raw_results -> List.map (fun instance -> Analyze.all ols instance raw_results) instances |> Analyze.merge ols instances) raw_results in
  let rect = { Bechamel_notty.w= 80; h= 1 } in
  List.iter (Notty_unix.(output_image <.> eol) <.> Bechamel_notty.Multiple.image_of_ols_results ~rect ~predictor:Measure.run) results
