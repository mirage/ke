open Bechamel
open Toolkit

let random ln =
  let ic = open_in "/dev/urandom" in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ;
  close_in ic ;
  Bytes.unsafe_to_string rs

let push_fke n =
  let raw = random n in
  let data = List.init n (String.get raw) in
  Staged.stage (fun () -> List.fold_left Ke.Fke.push Ke.Fke.empty data)

let push_rke n =
  let queue = Ke.Rke.create ~capacity:n () in
  let raw = random n in
  Staged.stage (fun () -> String.iter (Ke.Rke.push queue) raw)

let push_rke_n n =
  let queue = Ke.Rke.create ~capacity:n () in
  let raw = random n in
  let blit src src_off dst dst_off len = Bigstringaf.unsafe_blit_from_string src ~src_off dst ~dst_off ~len in
  Staged.stage (fun () -> Ke.Rke.N.push queue ~blit ~length:String.length raw)

let push_queue n =
  let queue = Queue.create () in
  let raw = random n in
  Staged.stage (fun () -> String.iter (fun chr -> Queue.add chr queue) raw)

let test_push_fke =
  Test.make_indexed ~name:"Fke.push"
    ~args:[100; 500; 1000; 5000; 10000;]
    push_fke

let test_push_rke =
  Test.make_indexed ~name:"Rke.push"
    ~args:[100; 500; 1000; 5000; 10000;]
    push_rke

let test_push_rke_n =
  Test.make_indexed ~name:"Rke.N.push"
    ~args:[100; 500; 1000; 5000; 10000;]
    push_rke_n

let test_push_queue =
  Test.make_indexed ~name:"Queue.push"
    ~args:[100; 500; 1000; 5000; 10000;]
    push_queue

let tests_push = [ test_push_fke; test_push_rke; test_push_rke_n; test_push_queue ]

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
      Fmt.pf ppf "%a per %a = %a" Label.pp
        (Analyze.OLS.responder result)
        Fmt.(Dump.list Label.pp)
        (Analyze.OLS.predictors result)
        Fmt.(styled style_by_r_square (Dump.list float))
        estimates
  | None ->
      Fmt.pf ppf "%a per %a = #unable-to-compute" Label.pp
        (Analyze.OLS.responder result)
        Fmt.(Dump.list Label.pp)
        (Analyze.OLS.predictors result)

let pp_ransac_result ppf result =
  Fmt.pf ppf "%a per %a = %04.0f [error: %04.0f]"
    Label.pp (Analyze.RANSAC.responder result)
    Label.pp (Analyze.RANSAC.predictor result)
    (Analyze.RANSAC.mean result)
    (Analyze.RANSAC.error result)

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let pp_ols_results ppf (test, results) =
  let tests = Test.set test in
  List.iter
    (fun results ->
      List.iter
        (fun (test, result) ->
          Fmt.pf ppf "@[<hov>%s = %a@]@\n"
            (pad 30 @@ Test.Elt.name test)
            pp_ols_result result )
        (zip tests results) )
    results

let pp_ransac_results ppf (test, results) =
  let tests = Test.set test in
  List.iter
    (fun results ->
      List.iter
        (fun (test, result) ->
          Fmt.pf ppf "@[<hov>%s = %a@]@\n"
            (pad 30 @@ Test.Elt.name test)
            pp_ransac_result result )
        (zip tests results) )
    results

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

let unzip =
  let rec go (l1, l2) = function
    | [] -> List.rev l1, List.rev l2
    | (x, y) :: r -> go (x :: l1, y :: l2) r in
  go ([], [])

let () =
  let ols = Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:Measure.[|run|] in
  let ransac = Analyze.ransac ~filter_outliers:true ~predictor:Measure.run in
  let instances = Instance.[minor_allocated; major_allocated; monotonic_clock] in
  let tests =
    match Sys.argv with
    | [|_|] -> []
    | [|_; "push"|] -> tests_push
    | [|_; "all"|] -> tests_push
    | _ -> Fmt.invalid_arg "%s {push|all}" Sys.argv.(1)
  in
  let measure_and_analyze test =
    let results =
      Benchmark.all ~stabilize:true ~quota:(Benchmark.s 2.) ~run:5000 instances test
    in
    List.map
      (fun x -> List.map (Analyze.analyze ols (Measure.label x)) results)
      instances,
    List.map
      (fun x -> List.map (Analyze.analyze ransac (Measure.label x)) results)
      instances
  in
  let ols_results, ransac_results = unzip (List.map measure_and_analyze tests) in
  List.iter
    (fun (test, result) ->
      Fmt.pr "[OLS] ---------- %s ----------\n%!" (Test.name test) ;
      Fmt.pr "%a\n%!" pp_ols_results (test, result) )
    (zip tests ols_results) ;
  List.iter
    (fun (test, result) ->
       Fmt.pr "[RANSAC] ---------- %s ----------\n%!" (Test.name test) ;
       Fmt.pr "%a\n%!" pp_ransac_results (test, result) )
    (zip tests ransac_results)


