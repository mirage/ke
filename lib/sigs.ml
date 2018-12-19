module type F = sig
  (** Queue containing element of type ['a]. *)
  type 'a t

  (** Raised when {!peek_end} or {!pop_exn} is applied to an empty queue. *)
  exception Empty

  val empty : 'a t
  (** An empty queue. *)

  val is_empty : 'a t -> bool
  (** Return [true] if the given queue is empty, [false] otherwise. *)

  val length : 'a t -> int
  (** Number of elements in the queue. *)

  val push : 'a t -> 'a -> 'a t
  (** Push element at the end of the queue. *)

  val cons : 'a t -> 'a -> 'a t
  (** Push element at the front of the queue. *)

  val peek : 'a t -> 'a option
  (** [peek q] returns the first element in the queue [q], without removing it
     from the queue. If [q] is empty, it returns [None]. *)

  val peek_exn : 'a t -> 'a
  (** Same as {!peek} but it raises an exception if [q] is empty. *)

  val pop : 'a t -> ('a * 'a t) option
  (** Get and remove the first element. If [q] is empty, it returns [None]. *)

  val pop_exn : 'a t -> 'a * 'a t
  (** Same as {!pop} but it raises an exception if [q] is empty. *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f q] applies [f] in turn to all elements of [q], from the least
     recently entered to the most recently entered. The queue itself is
     unchanged. *)

  val fold : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
  (** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the
     list of [q]'s elements. The queue remains unchanged. *)
end

module type M = sig
  (** The type of queues containing elements of type ['a]. *)
  type 'a t

  (** Raised when {!peek_exn} or {!pop_exn} is applied to an empty queue. *)
  exception Empty

  val create : ?capacity:int -> unit -> 'a t
  (** Return a new queue, initially empty. *)

  val is_empty : 'a t -> bool
  (** Return [true] if the given queue is empty, [false] otherwise. *)

  val length : 'a t -> int

  val push : 'a t -> 'a -> unit
  (** [push q x] adds the element [x] at the end of the queue [q]. *)

  val cons : 'a t -> 'a -> unit

  val peek : 'a t -> 'a option
  (** [peek q] returns the first element in the queue [q], without removing it
     from the queue. If [q] is empty, it returns [None]. *)

  val peek_exn : 'a t -> 'a
  (** Same as {!peek} but it raises an exception if [q] is empty. *)

  val pop : 'a t -> 'a option
  (** [pop q] removes and returns the first element in queue [q]. If [q] is
     empty, it returns [None]. *)

  val pop_exn : 'a t -> 'a
  (** Same as {!pop} but it raises an exception if [q] is empty. *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f q] applies [f] in turn to all elements of [q], from the least
     recently entered to the most recently entered. The queue itself is
     unchanged. *)

  val fold : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
  (** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the
     list of [q]'s elements. The queue remains unchanged. *)
end

module type R = sig
  (** The type of queues containing elements of type ['a]. *)
  type ('a, 'b) t

  exception Empty

  val is_empty : ('a, 'b) t -> bool
  (** Return [true] if the given queue is empty, [false] otherwise. *)

  val create : ?capacity:int -> ('a, 'b) Bigarray.kind -> ('a, 'b) t
  (** Return a new queue, initially empty. *)

  val push : ('a, 'b) t -> 'a -> unit
  (** [push q x] adds the elements [x] at the end of the queue [q]. *)

  val pop : ('a, 'b) t -> 'a option
  (** [pop q] removes and returns the first element in queue [q]. If [q] is
     empty, it returns [None]. *)

  val pop_exn : ('a, 'b) t -> 'a
  val peek : ('a, 'b) t -> 'a option
  val peek_exn : ('a, 'b) t -> 'a

  val cons : ('a, 'b) t -> 'a -> unit
  (** Push element at the front of the queue. *)

  module N : sig
    type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
    type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit
    type 'a length = 'a -> int

    val push :
         ('a, 'b) t
      -> blit:('src, ('a, 'b) bigarray) blit
      -> length:'src length
      -> ?off:int
      -> ?len:int
      -> 'src
      -> unit

    val keep :
         ('a, 'b) t
      -> blit:(('a, 'b) bigarray, 'dst) blit
      -> length:'dst length
      -> ?off:int
      -> ?len:int
      -> 'dst
      -> unit

    val pop : ('a, 'b) t -> int -> unit
  end

  val iter : ('a -> unit) -> ('a, 'b) t -> unit
  (** [iter f q] applies [f] in turn to all elements of [q], from the least
     recently entered to the most recently entered. The queue itself is
     unchanged. *)

  val fold : ('acc -> 'x -> 'acc) -> 'acc -> ('x, 'b) t -> 'acc
  (** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the
     list of [q]'s elements. The queue remains unchanged. *)
end

module Weighted = struct
  module type R = sig
    (** The type of queues containing elements of type ['a]. *)
    type ('a, 'b) t

    exception Full
    exception Empty

    val is_empty : ('a, 'b) t -> bool
    (** Return [true] if the given queue is empty, [false] otherwise. *)

    val create : ?capacity:int -> ('a, 'b) Bigarray.kind -> ('a, 'b) t * int
    (** Return a new queue, initially empty. *)

    val push_exn : ('a, 'b) t -> 'a -> unit
    (** [push_exn q x] adds the elements [x] at the end of the queue [q]. *)

    val push : ('a, 'b) t -> 'a -> unit option

    val pop : ('a, 'b) t -> 'a option
    (** [pop q] removes and returns the first element in queue [q]. If [q] is
       empty, it returns [None]. *)

    val pop_exn : ('a, 'b) t -> 'a
    val peek : ('a, 'b) t -> 'a option
    val peek_exn : ('a, 'b) t -> 'a

    val cons_exn : ('a, 'b) t -> 'a -> unit
    (** Push element at the front of the queue. *)

    val cons : ('a, 'b) t -> 'a -> unit option

    val iter : ('a -> unit) -> ('a, 'b) t -> unit
    (** [iter f q] applies [f] in turn to all elements of [q], from the least
       recently entered to the most recently entered. The queue itself is
       unchanged. *)

    val fold : ('acc -> 'x -> 'acc) -> 'acc -> ('x, 'b) t -> 'acc
    (** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the
       list of [q]'s elements. The queue remains unchanged. *)
  end

  module type F = sig
    type ('a, 'b) t

    exception Empty
    exception Full

    val is_empty : ('a, 'b) t -> bool
    val create : ?capacity:int -> ('a, 'b) Bigarray.kind -> ('a, 'b) t * int
    val push : ('a, 'b) t -> 'a -> ('a, 'b) t option
    val pop : ('a, 'b) t -> ('a * ('a, 'b) t) option
    val pop_exn : ('a, 'b) t -> 'a * ('a, 'b) t
    val peek : ('a, 'b) t -> 'a option
    val peek_exn : ('a, 'b) t -> 'a
    val cons : ('a, 'b) t -> 'a -> ('a, 'b) t option
    val cons_exn : ('a, 'b) t -> 'a -> ('a, 'b) t

    module N : sig
      type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
      type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit
      type 'a length = 'a -> int

      val push_exn :
           ('a, 'b) t
        -> blit:('src, ('a, 'b) bigarray) blit
        -> length:'src length
        -> ?off:int
        -> ?len:int
        -> 'src
        -> ('a, 'b) bigarray list * ('a, 'b) t

      val push :
           ('a, 'b) t
        -> blit:('src, ('a, 'b) bigarray) blit
        -> length:'src length
        -> ?off:int
        -> ?len:int
        -> 'src
        -> (('a, 'b) bigarray list * ('a, 'b) t) option

      val keep_exn :
           ('a, 'b) t
        -> blit:(('a, 'b) bigarray, 'dst) blit
        -> length:'dst length
        -> ?off:int
        -> ?len:int
        -> 'dst
        -> unit

      val keep :
           ('a, 'b) t
        -> blit:(('a, 'b) bigarray, 'dst) blit
        -> length:'dst length
        -> ?off:int
        -> ?len:int
        -> 'dst
        -> unit option

      val unsafe_shift : ('a, 'b) t -> int -> ('a, 'b) t
      val shift_exn : ('a, 'b) t -> int -> ('a, 'b) t
      val shift : ('a, 'b) t -> int -> ('a, 'b) t option
    end

    val iter : ('a -> unit) -> ('a, 'b) t -> unit
    val fold : ('acc -> 'x -> 'acc) -> 'acc -> ('x, 'b) t -> 'acc
  end
end
