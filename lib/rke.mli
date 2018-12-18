type ('a, 'b) t
(** The type of queues containing elements of type [char]. *)

exception Empty

val is_empty : ('a, 'b) t -> bool
(** Return [true] if the given queue is empty, [false] otherwise. *)

val create : ?capacity:int -> ('a, 'b) Bigarray.kind -> ('a, 'b) t
(** Return a new queue, initially empty. *)

val push : ('a, 'b) t -> 'a -> unit
(** [push q x] adds the elements [x] at the end of the queue [q]. *)

val pop : ('a, 'b) t -> 'a option
(** [pop q] removes and returns the first element in queue [q]. If [q] is empty,
    it returns [None]. *)

val pop_exn : ('a, 'b) t -> 'a

val peek : ('a, 'b) t -> 'a option

val peek_exn : ('a, 'b) t -> 'a

val cons : ('a, 'b) t -> 'a -> unit
(** Push element at the front of the queue. *)

module N : sig
  type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit
  type 'a length = 'a -> int

  val push : ('a, 'b) t -> blit:('src, ('a, 'b) bigarray) blit -> length:'src length -> ?off:int -> ?len:int -> 'src -> unit

  val keep : ('a, 'b) t -> blit:(('a, 'b) bigarray, 'dst) blit -> length:'dst length -> ?off:int -> ?len:int -> 'dst -> unit

  val pop : ('a, 'b) t -> int -> unit
end

val iter : ('a -> unit) -> ('a, 'b) t -> unit
(** [iter f q] applies [f] in turn to all elements of [q], from the least
   recently entered to the most recently entered. The queue itself is unchanged.
   *)

val fold : ('acc -> 'x -> 'acc) -> 'acc -> ('x, 'b) t -> 'acc
(** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the list
   of [q]'s elements. The queue remains unchanged. *)
