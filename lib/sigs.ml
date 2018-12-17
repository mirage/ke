module type F = sig
  type 'a t

  exception Empty

  val empty : 'a t

  val is_empty : 'a t -> bool
  val length : 'a t -> int

  val push : 'a t -> 'a -> 'a t
  val cons : 'a t -> 'a -> 'a t
  val peek : 'a t -> 'a option
  val peek_exn : 'a t -> 'a
  val pop : 'a t -> ('a * 'a t) option
  val pop_exn : 'a t -> 'a * 'a t

  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
end

module type M = sig
  type 'a t
  (** The type of queues containing elements of type ['a]. *)

  exception Empty
  (** Raised when {!peek_exn} or {!pop_exn} is applied to an empty queue. *)

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
