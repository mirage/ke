let () = Printexc.record_backtrace true

module type GRAPH = sig
  type t
  type id
  type value

  type error = private [> `Not_found ]

  val pp_error : error Fmt.t
  val preds : value -> id list
  val get : t -> id -> (value, error) result Lwt.t
  val compare : value -> value -> int
end

module Make (Q: Ke.Sigs.R) (G: GRAPH with type id = nativeint) = struct
  open Lwt

  exception Graph of G.error

  module Node = struct
    type t = {value: G.value; mutable color: [`White | `Black] }

    let compare a b =
      G.compare a.value b.value
  end

  module Pq = Psq.Make(Nativeint)(Node)
  module Map = Map.Make(Nativeint)

  let pack graph exclude roots =
    let store = Hashtbl.create 128 in
    let memoize get id =
      try
        let ret = Hashtbl.find store id in
        Lwt.return (Some ret)
      with Not_found ->
        get id >>= function
        | Ok value ->
          let node = {Node.value; color= `White} in
          Hashtbl.add store id node ;
          Lwt.return (Some node)
        | Error `Not_found -> Lwt.return None
        | Error err -> Lwt.fail (Graph err) in
    let preds v = G.preds v in
    let get = memoize (G.get graph) in
    let all_blacks pq =
      Pq.fold
        (fun _ -> function {Node.color= `Black; _} -> ( && ) true
                         | _ -> ( && ) false)
        true pq in
    let propagate {Node.value; color} =
      let p = preds value in
      let q = Q.create ~capacity:(List.length p) Bigarray.Nativeint in
      let rec go () =
        match Q.pop q with
        | Some id ->
          (try
             let node = Hashtbl.find store id in
             node.Node.color <- color ;
             go (List.iter (Q.push q) (preds node.Node.value))
           with Not_found -> go ())
        | None -> () in
      List.iter (Q.push q) p ; go () in
    let rec garbage pq =
      if all_blacks pq then Lwt.return ()
      else match Pq.pop pq with
        | Some ((_, {Node.value; color = `Black}), pq) ->
          Lwt_list.fold_left_s
            (fun pq id ->
               get id >>= function
               | Some ({Node.color= `White; _} as node) ->
                 node.Node.color <- `Black ;
                 propagate node ;
                 Lwt.return (Pq.add id node pq)
               | Some node -> Lwt.return (Pq.add id node pq)
               | None -> Lwt.return pq)
            pq (preds value) >>= garbage
        | Some ((_, {Node.value; _}), pq) ->
          Lwt_list.fold_left_s
            (fun pq id ->
               get id >>= function
               | None -> Lwt.return pq
               | Some node -> Lwt.return (Pq.add id node pq))
            pq (preds value) >>= garbage
        | None -> Lwt.return () in
    let collect () =
      Hashtbl.fold (fun id -> function
          | {Node.color= `White; value} -> Map.add id value
          | _ -> fun acc -> acc)
        store Map.empty in
    Lwt_list.map_s
      (fun id -> get id >>= function
         | Some node -> Lwt.return (Some (id, node))
         | None -> Lwt.return None)
      roots
    >>= fun roots ->
    Lwt_list.map_s
      (fun id ->
         get id >>= function
         | Some node ->
           node.Node.color <- `Black ;
           Lwt.return (Some (id, node))
         | None -> Lwt.return None)
      exclude
    >|= List.append roots
    >|= List.fold_left (fun acc -> function None -> acc | Some x -> x :: acc) []
    >|= Pq.of_list
    >>= fun pq -> garbage pq >|= collect
end

module Git = struct
  type t = unit
  type id = nativeint
  type value = {name: string; time: int64; ancestors: nativeint list}

  type error = [ `Not_found ]

  let pp_error ppf = function
    | `Not_found -> Fmt.string ppf "`Not_found"

  let store : (id, value) Hashtbl.t = Hashtbl.create 16

  let preds {ancestors; _} = ancestors
  let get () id = try Lwt.return_ok (Hashtbl.find store id) with Not_found -> Lwt.return_error `Not_found
  let compare {time= a; _} {time=b; _ } = Int64.compare a b
end

module Packer = Make(Ke.Rke)(Git)

let json =
  let open Json_encoding in
  let name = req "name" string in
  let time = req "time" (conv Int64.to_string Int64.of_string string) in
  let ancestors = req "ancestors" (list (conv Nativeint.to_int32 Nativeint.of_int32 int32)) in
  conv
    (fun {Git.name; time; ancestors;} -> (name, time, ancestors))
    (fun (name, time, ancestors) -> {Git.name; time; ancestors})
    (obj3 name time ancestors)

type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]
type value = [ `Null | `Bool of bool | `String of string | `Float of float ]

let pp_json ppf v =
  let rec pp_value ppf = function
    | `Bool v -> Fmt.bool ppf v
    | `String v -> Fmt.quote Fmt.text ppf v
    | `Float v -> Fmt.float ppf v
    | `Null -> Fmt.string ppf "<null>"
    | `A l -> pp_arr ppf l
    | `O l -> pp_obj ppf l
  and pp_arr ppf arr = Fmt.(using Array.of_list (Dump.array pp_value)) ppf arr
  and pp_obj ppf obj = Fmt.Dump.iter_bindings (fun f -> List.iter (fun (k, v) -> f k v)) Fmt.(always "object") Fmt.string pp_value ppf obj in
  pp_value ppf v

let of_database ic : Git.value list =
  let decoder = Jsonm.decoder (`Channel ic) in

  let error (`Error err) = Fmt.invalid_arg "%a" Jsonm.pp_error err in
  let end_of_input `End = Fmt.invalid_arg "Unexpected end of input" in

  let rec arr acc k = match Jsonm.decode decoder with
    | #await -> assert false
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> base (fun v -> arr (v :: acc) k) v

  and name n k = match Jsonm.decode decoder with
    | #await -> assert false
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme v -> base (fun v -> k (n, v)) v

  and obj acc k = match Jsonm.decode decoder with
    | #await -> assert false
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v -> Fmt.invalid_arg "Unexpected lexeme: %a" Jsonm.pp_lexeme v

  and base k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> Fmt.invalid_arg "Unexpected end of array/object"
    | `Name n -> Fmt.invalid_arg "Unexpected key: %s" n in

  let go k = match Jsonm.decode decoder with
    | #await -> assert false
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme (#Jsonm.lexeme as lexeme) -> base k lexeme in

  go Json_encoding.(destruct (list json))

let flat_json json : Jsonm.lexeme list =
  let rec arr acc k = function
    | [] -> k (List.rev (`Ae :: acc))
    | (#value as x) :: r -> arr (x :: acc) k r
    | `A l :: r -> arr [ `As ] (fun l -> arr (List.rev_append l acc) k r) l
    | `O l :: r -> obj [ `Os ] (fun l -> arr (List.rev_append l acc) k r) l

  and obj acc k = function
    | [] -> k (List.rev (`Oe :: acc))
    | (n, x) :: r -> base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x

  and base k = function
    | `A l -> arr [ `As ] k l
    | `O l -> obj [ `Os ] k l
    | #value as x -> k [ x ] in

  base (fun l -> l) json

external identity : 'a -> 'a = "%identity"

let show ppf map =
  let json = Json_encoding.(construct (list json) (List.map snd (Packer.Map.bindings map))) in
  let raw = Bytes.create 0x800 in
  let encoder = Jsonm.encoder `Manual in
  let rec write k = function
    | `Ok -> k ()
    | `Partial ->
      Fmt.string ppf (Bytes.sub_string raw 0 (Jsonm.Manual.dst_rem encoder)) ;
      Jsonm.Manual.dst encoder raw 0 (Bytes.length raw) ;
      write k (Jsonm.encode encoder `Await) in
  let rec go k = function
    | [] -> write k (Jsonm.encode encoder `End)
    | lexeme :: r -> write (fun () -> go k r) (Jsonm.encode encoder (`Lexeme lexeme)) in
  let lexemes = flat_json json in
  go identity lexemes

let run database exclude roots =
  let graph = of_database (open_in database) in
  let () = List.iteri (fun id value -> Hashtbl.add Git.store (Nativeint.of_int id) value) graph in
  match Lwt_main.run Lwt.Infix.(Packer.pack () exclude roots >|= Fmt.fmt "%a\n%!" Fmt.stdout show) with
  | () -> `Ok ()
  | exception Packer.Graph err -> `Error (false, Fmt.strf "Retrieve an error: %a." Git.pp_error err)

open Cmdliner

let database =
  let parser s =
    if Sys.file_exists s
    then Ok s
    else Rresult.R.error_msgf "File %s does not exists" s in
  let pp = Fmt.string in
  Arg.conv (parser, pp)

let id =
  let parser s =
    match Nativeint.of_string_opt s with
    | Some n -> Ok n
    | None -> Rresult.R.error_msgf "Invalid id: %s" s in
  let pp = Fmt.nativeint in
  Arg.conv (parser, pp)

let database =
  let doc = "Database of graph." in
  Arg.(required & opt (some database) None & info ["d"; "database"] ~doc ~docv:"<FILE>")

let roots =
  let doc = "Roots of graph." in
  Arg.(non_empty & opt (list id) [] & info ["r"; "roots"] ~doc ~docv:"<LST>")

let exclude =
  let doc = "Excluded nodes of graph." in
  Arg.(non_empty & opt (list id) [] & info ["e"; "exclude"] ~doc ~docv:"<LST>")

let command =
  let doc = "Example of ke." in
  let exits = Term.default_exits in
  Term.(ret (const run $ database $ exclude $ roots), Term.info "packer" ~version:"dev" ~doc ~exits)

let () = Term.(exit @@ eval command)
