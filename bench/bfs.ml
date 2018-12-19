module type F_PROBLEM = sig
  type state
  type move

  val success : state -> bool
  val moves : state -> (move * state) list

  type table

  val create : unit -> table
  val add : table -> state -> unit
  val mem : table -> state -> bool
  val clear : table -> unit
end

module type M_PROBLEM = sig
  type move

  val success : unit -> bool
  val moves : unit -> move list
  val do_move : move -> unit
  val undo_move : move -> unit
  val add : unit -> unit
  val mem : unit -> bool
  val clear : unit -> unit
end

module F (Q : Ke.Sigs.M) (P : F_PROBLEM) = struct
  let search s0 =
    let visited = P.create () in
    let already s = P.mem visited s || (P.add visited s ; false) in
    let _ = already s0 in
    let q = Q.create () in
    Q.push q ([], s0) ;
    let rec bfs () =
      if Q.is_empty q then raise Not_found ;
      let path, s = Q.pop_exn q in
      if P.success s then (s, List.rev path)
      else (
        List.iter
          (fun (m, s') -> if not (already s') then Q.push q (m :: path, s'))
          (P.moves s) ;
        bfs () )
    in
    bfs ()
end

module M (Q : Ke.Sigs.M) (P : M_PROBLEM) = struct
  let rec cut_head n l = if n == 0 then l else cut_head (pred n) (List.tl l)

  let common_psuffix (n1, l1) (n2, l2) =
    let rec suffix l1 l2 =
      if l1 == l2 then l1 else suffix (List.tl l1) (List.tl l2)
    in
    if n1 < n2 then suffix l1 (cut_head (n2 - n1) l2)
    else if n2 < n1 then suffix (cut_head (n1 - n2) l1) l2
    else suffix l1 l2

  let search () =
    let already () = P.mem () || (P.add () ; false) in
    let q = Q.create () in
    Q.push q (0, []) ;
    let cpath = ref (0, []) in
    let rec restore_state path =
      let suf = common_psuffix path !cpath in
      let rec backward = function
        | m :: r as p when p != suf -> P.undo_move m ; backward r
        | _ -> ()
      in
      let rec forward = function
        | m :: r as p when p != suf -> forward r ; P.do_move m
        | _ -> ()
      in
      backward (snd !cpath) ;
      forward (snd path) ;
      cpath := path
    in
    let rec bfs () =
      if Q.is_empty q then raise Not_found ;
      let ((n, path) as s) = Q.pop_exn q in
      restore_state s ;
      if P.success () then List.rev path
      else if not (already ()) then (
        List.iter (fun m -> Q.push q (succ n, m :: path)) (P.moves ()) ;
        bfs () )
      else bfs ()
    in
    bfs ()
end
