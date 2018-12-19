open Toolkit
open Bechamel

module Realtime_clock = struct
  type label = string
  type witness = unit
  type value = int64 ref

  let load () = ()
  let unload () = ()
  let make () = ()
  let label () = "realtime-clock"
  let epsilon () = {contents= 0L}
  let blit () v = v := Oclock.gettime Oclock.realtime
  let diff a b = {contents= Int64.sub !b !a}
  let float x = Int64.to_float !x
end

module Extension = struct
  include Extension

  let realtime_clock = Measure.make (module Realtime_clock)
end

module Instance = struct
  include Instance

  let realtime_clock =
    Measure.instance (module Realtime_clock) Extension.realtime_clock
end

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

let zip l1 l2 =
  let rec go acc = function
    | [], [] -> List.rev acc
    | x1 :: r1, x2 :: r2 -> go ((x1, x2) :: acc) (r1, r2)
    | _, _ -> assert false
  in
  go [] (l1, l2)

let pp_ols_result ppf result =
  let style_by_r_square =
    match Analyze.OLS.r_square result with
    | Some r_square ->
        if r_square >= 0.95 then `Green
        else if r_square >= 0.90 then `Yellow
        else `Red
    | None -> `None
  in
  match Analyze.OLS.estimates result with
  | Some estimates ->
      Fmt.pf ppf "%a [r²: %a]"
        Fmt.(styled style_by_r_square (Dump.list float))
        estimates
        Fmt.(option float)
        (Analyze.OLS.r_square result)
  | None -> Fmt.pf ppf "#unable-to-compute"

let pp_ransac_result ppf result =
  Fmt.pf ppf "%04.04f [error: %04.04f]"
    (Analyze.RANSAC.mean result)
    (Analyze.RANSAC.error result)

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let pp_ols_results : (string, Analyze.OLS.t) Hashtbl.t Fmt.t =
 fun ppf ->
  Hashtbl.iter (fun test_name result ->
      Fmt.pf ppf "@[<hov>[ols]%s = %a@]@\n"
        (pad 30 @@ test_name)
        pp_ols_result result )

let pp_ransac_results : (string, Analyze.RANSAC.t) Hashtbl.t Fmt.t =
 fun ppf ->
  Hashtbl.iter (fun test_name result ->
      Fmt.pf ppf "@[<hov>[ransac]%s = %a@]@\n"
        (pad 30 @@ test_name)
        pp_ransac_result result )

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let with_src_and_stamp h _ k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf
        ("%s %a %a: @[" ^^ fmt ^^ "@]@.")
        (pad 20 (Fmt.strf "%+04.0fus" dt))
        Logs_fmt.pp_header (level, h)
        Fmt.(styled `Magenta string)
        (pad 20 @@ Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_src_and_stamp header tags k fmt
  in
  {Logs.report}

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stdout) ;
  let quiet = match style_renderer with Some _ -> true | None -> false in
  (quiet, Fmt.stdout)

let _, _ = setup_logs (Some `Ansi_tty) (Some Logs.Debug)

let () =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:Measure.[|run|]
  in
  let ransac = Analyze.ransac ~filter_outliers:true ~predictor:Measure.run in
  let instances =
    Instance.[minor_allocated; major_allocated; cpu_clock; realtime_clock]
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
  let ols_results = Hashtbl.create (List.length instances) in
  let ransac_results = Hashtbl.create (List.length instances) in
  let () =
    List.iter
      (fun x -> Hashtbl.add ols_results (Measure.label x) (Hashtbl.create 16))
      instances
  in
  let () =
    List.iter
      (fun x ->
        Hashtbl.add ransac_results (Measure.label x) (Hashtbl.create 16) )
      instances
  in
  let measure_and_analyze test =
    let results =
      Benchmark.all ~stabilize:true ~quota:(Benchmark.s 2.) ~run:5000 instances
        test
    in
    List.iter
      (fun x ->
        let r = Analyze.all ols x results in
        Hashtbl.add
          (Hashtbl.find ols_results (Measure.label x))
          (Test.name test) r )
      instances ;
    List.iter
      (fun x ->
        let r = Analyze.all ransac x results in
        Hashtbl.add
          (Hashtbl.find ransac_results (Measure.label x))
          (Test.name test) r )
      instances
  in
  let () = List.iter measure_and_analyze tests in
  Hashtbl.iter
    (fun label results ->
      Fmt.pr "%a: @[<v>%a@]\n%!" Label.pp label
        Fmt.(hashtbl (using snd pp_ols_results))
        results )
    ols_results ;
  Hashtbl.iter
    (fun label results ->
      Fmt.pr "%a: @[<v>%a@]\n%!" Label.pp label
        Fmt.(hashtbl (using snd pp_ransac_results))
        results )
    ransac_results
