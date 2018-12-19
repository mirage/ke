(* XXX(dinosaure):
   - ['a Cheap_option.t Cheap_option.t] does not work!
   - [float Cheap_option.t array] does not work! *)

module Cheap_option : sig
  type +'a t

  val none : _ t
  val some : 'a -> 'a t
  val value_exn : 'a t -> 'a
end = struct
  let hack (type a b) (a : a) (b : b) = Pervasives.( == ) a (Obj.magic b : a)

  module X : sig
    type +'a t

    val none : _ t
    val some : 'a -> 'a t
    val value_exn : 'a t -> 'a
  end = struct
    type +'a t

    let none_substitute : _ t = Obj.obj (Obj.new_block Obj.abstract_tag 1)
    let none : _ t = Obj.magic `ca113c5fe58a8127
    let is_some x = not (hack x none)

    let some (type a) (x : a) : a t =
      if hack x none then none_substitute else Obj.magic x

    let value_unsafe (type a) (x : a t) : a =
      if hack x none_substitute then Obj.magic none else Obj.magic x

    let value_exn x =
      if is_some x then value_unsafe x else failwith "Cheap_option.value_exn"
  end

  include X
end

module Obj_array : sig
  type t

  val create : int -> Obj.t -> t
  val length : t -> int
  val unsafe_get : t -> int -> Obj.t
  val unsafe_set : t -> int -> Obj.t -> unit
  val unsafe_blit : t -> int -> t -> int -> int -> unit
end = struct
  type t = Obj.t array

  external create : int -> 'a -> 'a array = "caml_make_vect"

  let zero_obj = Obj.repr (0 : int)
  let create_null len = create len zero_obj

  let create len x =
    if Obj.tag x <> Obj.double_tag then create len x
    else
      let t = create_null len in
      let x = Sys.opaque_identity x in
      for i = 0 to len - 1 do
        Array.unsafe_set t i x
      done ;
      t

  let length x = Array.length x

  type not_a_float = Not_a_float_0 | Not_a_float_1 of int

  let _not_a_float_0 = Not_a_float_0
  let _not_a_float_1 = Not_a_float_1 42

  let unsafe_get t i =
    Obj.repr
      (Array.unsafe_get (Obj.magic (t : t) : not_a_float array) i : not_a_float)

  let[@inline always] unsafe_set_with_caml_modify t i o =
    Array.unsafe_set
      (Obj.magic (t : t) : not_a_float array)
      i
      (Obj.obj (Sys.opaque_identity o) : not_a_float)

  let[@inline always] unsafe_set_assuming_int t i x =
    Array.unsafe_set (Obj.magic (t : t) : int array) i (Sys.opaque_identity x)

  let unsafe_set t i x =
    let old = unsafe_get t i in
    if Obj.is_int old && Obj.is_int x then
      unsafe_set_assuming_int t i (Obj.obj x : int)
    else unsafe_set_with_caml_modify t i x

  let unsafe_blit src src_pos dst dst_pos len =
    if dst_pos < src_pos then
      for i = 0 to len - 1 do
        unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
      done
    else
      for i = len - 1 downto 0 do
        unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
      done
end

module Uniform_array : sig
  type 'a t

  val create : int -> 'a -> 'a t
  val length : 'a t -> int
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
  val unsafe_blit : 'a t -> int -> 'a t -> int -> int -> unit
end = struct
  module X : sig
    type 'a t

    val create : int -> 'a -> 'a t
    val unsafe_get : 'a t -> int -> 'a
    val unsafe_set : 'a t -> int -> 'a -> unit
    val unsafe_blit : 'a t -> int -> 'a t -> int -> int -> unit
    val length : 'a t -> int
  end = struct
    type 'a t = Obj_array.t

    let create len x = Obj_array.create len (Obj.repr x)
    let unsafe_get arr i = Obj.obj (Obj_array.unsafe_get arr i)
    let unsafe_set arr i x = Obj_array.unsafe_set arr i (Obj.repr x)

    let unsafe_blit src src_pos dst dst_pos len =
      Obj_array.unsafe_blit src src_pos dst dst_pos len

    let length t = Obj_array.length t
  end

  include X
end

module Option_array : sig
  type 'a t

  val create : int -> 'a t
  val length : 'a t -> int
  val unsafe_get_some_exn : 'a t -> int -> 'a
  val unsafe_set_some : 'a t -> int -> 'a -> unit
  val unsafe_set_none : 'a t -> int -> unit
  val unsafe_blit : 'a t -> int -> 'a t -> int -> int -> unit
end = struct
  type 'a t = 'a Cheap_option.t Uniform_array.t

  let create len = Uniform_array.create len Cheap_option.none
  let length t = Uniform_array.length t

  let unsafe_get_some_exn t i =
    Cheap_option.value_exn (Uniform_array.unsafe_get t i)

  let unsafe_set_some t i x =
    Uniform_array.unsafe_set t i (Cheap_option.some x)

  let unsafe_set_none t i = Uniform_array.unsafe_set t i Cheap_option.none

  let unsafe_blit src src_pos dst dst_pos len =
    Uniform_array.unsafe_blit src src_pos dst dst_pos len
end

type 'a t =
  {mutable r: int; mutable w: int; mutable c: int; mutable v: 'a Option_array.t}

external ( = ) : 'a -> 'a -> bool = "%equal"

let ( = ) (a : int) b = a = b
let[@inline always] mask t v = v land (t.c - 1)
let[@inline always] empty t = t.r = t.w
let[@inline always] size t = t.w - t.r
let[@inline always] full t = size t = t.c
let length t = (size [@inlined]) t

let[@inline always] to_power_of_two v =
  let res = ref (pred v) in
  res := !res lor (!res lsr 1) ;
  res := !res lor (!res lsr 2) ;
  res := !res lor (!res lsr 4) ;
  res := !res lor (!res lsr 8) ;
  res := !res lor (!res lsr 16) ;
  succ !res

let is_empty t = (empty [@inlined]) t

let create ?capacity () =
  let capacity =
    match capacity with
    | None | Some 0 -> 1
    | Some n ->
        if n < 0 then Fmt.invalid_arg "Rke.create" else to_power_of_two n
  in
  {r= 0; w= 0; c= capacity; v= Option_array.create capacity}

let grow t want =
  let max : int -> int -> int = max in
  let c = to_power_of_two (max 1 (max want (size t))) in
  if c <> Option_array.length t.v then (
    let dst = Option_array.create c in
    let sze = (size [@inlined]) t in
    let msk = (mask [@inlined]) t t.r in
    let pre = t.c - msk in
    let rst = sze - pre in
    if rst > 0 then (
      Option_array.unsafe_blit t.v msk dst 0 pre ;
      Option_array.unsafe_blit t.v 0 dst pre rst )
    else Option_array.unsafe_blit t.v msk dst 0 sze ;
    t.v <- dst ;
    t.w <- sze ;
    t.c <- c ;
    t.r <- 0 )

let push t v =
  if (full [@inlined]) t then grow t (2 * (size [@inlined]) t) ;
  Option_array.unsafe_set_some t.v ((mask [@inlined]) t t.w) v ;
  t.w <- t.w + 1

let cons t v =
  if (full [@inlined]) t then grow t (2 * (size [@inlined]) t) ;
  let i = t.r - 1 in
  Option_array.unsafe_set_some t.v ((mask [@inlined]) t i) v ;
  t.r <- i

exception Empty

let pop_exn t =
  if (empty [@inlined]) t then raise Empty ;
  let m = (mask [@inlined]) t t.r in
  let r = Option_array.unsafe_get_some_exn t.v m in
  Option_array.unsafe_set_none t.v m ;
  t.r <- t.r + 1 ;
  r

let pop t = try Some (pop_exn t) with Empty -> None

let peek_exn t =
  if (empty [@inlined]) t then raise Empty ;
  Option_array.unsafe_get_some_exn t.v ((mask [@inlined]) t t.r)

let peek t = try Some (peek_exn t) with Empty -> None

let iter f t =
  let idx = ref t.r in
  let max = t.w in
  while !idx <> max do
    f (Option_array.unsafe_get_some_exn t.v ((mask [@inlined]) t !idx)) ;
    incr idx
  done

let fold f a t =
  let a = ref a in
  iter (fun x -> a := f !a x) t ;
  !a
