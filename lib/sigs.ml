module type F = sig
  (** The type of queues containing elements of type ['a]. *)
  type 'a t

  (** Raised when {!peek_exn} or {!pop_exn} is applied to an empty queue. *)
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

  val tail : 'a t -> ('a t * 'a) option
  (** Get and remove the {b last} element. If [q] is empty, it returns [None]. *)

  val tail_exn : 'a t -> 'a t * 'a
  (** Same as {!tail} but it raises an exception if [q] is empty. *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f q] applies [f] in turn to all elements of [q], from the least
     recently entered to the most recently entered. The queue itself is
     unchanged. *)

  val rev_iter : ('a -> unit) -> 'a t -> unit
  (** [rev_iter f q] applies [f] in turn to all elements of [q], from the most
     recently entered to the least recently entered. The queue itself is
     unchanged. *)

  val fold : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
  (** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the
     list of [q]'s elements. The queue remains unchanged. *)

  val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
  (** Pretty-printer of {!t}. *)

  val dump : 'a Fmt.t -> 'a t Fmt.t
  (** Human-readable pretty-printer of {!t}. *)
end

module type R = sig
  (** The type of queues containing elements of type ['a]. *)
  type ('a, 'b) t

  (** Raised when {!peek_exn}, {!pop_exn}, {!N.keep_exn} or {!N.shift_exn} is
     applied to an empty queue. *)
  exception Empty

  val is_empty : ('a, 'b) t -> bool
  (** Return [true] if the given queue is empty, [false] otherwise. *)

  val create : ?capacity:int -> ('a, 'b) Bigarray_compat.kind -> ('a, 'b) t
  (** Return a new queue, initially empty. *)

  val capacity : ('a, 'b) t -> int
  (** Returns how many objects [t] can store. *)

  val length : ('a, 'b) t -> int
  (** Number of elements in the queue. *)

  val push : ('a, 'b) t -> 'a -> unit
  (** [push q x] adds the elements [x] at the end of the queue [q]. *)

  val pop : ('a, 'b) t -> 'a option
  (** [pop q] removes and returns the first element in queue [q]. If [q] is
     empty, it returns [None]. *)

  val pop_exn : ('a, 'b) t -> 'a
  (** [pop_exn] is the same as {!pop} but it raises {!Empty} when the given
     queue [q] is empty. *)

  val peek : ('a, 'b) t -> 'a option
  (** [peek q] returns the first element in the queue [q], without removing it
     from the queue. If [q] is empty, it returns [None]. *)

  val peek_exn : ('a, 'b) t -> 'a
  (** Same as {!peek} but it raises {!Empty} if [q] is empty. *)

  val cons : ('a, 'b) t -> 'a -> unit
  (** [cons q x] adds element [x] at the front of the given queue [q]. It
     returns [None] if it fails. *)

  val copy : ('a, 'b) t -> ('a, 'b) t
  (** Return a copy of the given queue. *)

  val clear : ('a, 'b) t -> unit
  (** Discard all elements from a queue. *)

  val compress : ('a, 'b) t -> unit
  (** Compress queue, read cursor will be setted to [0] and data will be move
     to. This operation allows to provide much more space for a
     {!push}/{!N.push} operation - but it can not ensure enough free space. *)

  module N : sig
    (** The type of the internal bigarray of {!t}. *)
    type ('a, 'b) bigarray = ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t

    (** The type of the [blit] function. *)
    type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit

    (** The type of the [length] function. *)
    type 'a length = 'a -> int

    val push :
         ('a, 'b) t
      -> blit:('src, ('a, 'b) bigarray) blit
      -> length:'src length
      -> ?off:int
      -> ?len:int
      -> 'src
      -> unit
    (** [push q ~blit ~length ?off ?len src] {i blits} elements in [src] to the
       given queue [q] at the end (like a fast iterative {!R.push}). Default
       value of [off] is [0]. Default value of [len] is [length src - off]. *)

    val keep_exn :
         ('a, 'b) t
      -> blit:(('a, 'b) bigarray, 'dst) blit
      -> length:'dst length
      -> ?off:int
      -> ?len:int
      -> 'dst
      -> unit
    (** [keep_exn q ~blit ~length ?off ?len dst] {i blits} elements of the given
       queue [q] in [dst] from the front to the end of [dst] (like a fast
       iterative {!R.pop_exn}). Default value of [off] is [0]. Default value of
       [len] is [length dst - off]. If the given [q] does not have enough
       elements to write on [dst], it raises {!Empty} and the given queue is
       unchanged. *)

    val keep :
         ('a, 'b) t
      -> blit:(('a, 'b) bigarray, 'dst) blit
      -> length:'dst length
      -> ?off:int
      -> ?len:int
      -> 'dst
      -> unit option
    (** Same as {!keep_exn} but if it fails, it returns [None]. *)

    val peek : ('a, 'b) t -> ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t list
    (** Returns a sub-part of available to read payloads. *)

    val unsafe_shift : ('a, 'b) t -> int -> unit
    (** [unsafe_shift q l] discards [l] elements in the given queue [q] without
       any verification. Mostly used after {!keep_exn}, if the last one does not
       raise {!Empty}, it's safe to use it. *)

    val shift_exn : ('a, 'b) t -> int -> unit
    (** [shift_exn q l] discards [l] elements in the given queue [q]. If [q]
       does not have enough elements, it raises {!Empty} and the given queue is
       unchanged. *)

    val shift : ('a, 'b) t -> int -> unit option
    (** Same as {!shift_exn} but if it fails, it returns [None]. *)
  end

  val iter : ('a -> unit) -> ('a, 'b) t -> unit
  (** [iter f q] applies [f] in turn to all elements of [q], from the least
     recently entered to the most recently entered. The queue itself is
     unchanged. *)

  val rev_iter : ('a -> unit) -> ('a, 'b) t -> unit
  (** [iter f q] applies [f] in turn to all elements of [q], from the most
     recently entered to the least recently entered. The queue itself is
     unchanged. *)

  val fold : ('acc -> 'x -> 'acc) -> 'acc -> ('x, 'b) t -> 'acc
  (** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the
     list of [q]'s elements. The queue remains unchanged. *)

  val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> ('a, 'b) t Fmt.t
  (** Pretty-printer of {!t}. *)

  val dump : 'a Fmt.t -> ('a, 'b) t Fmt.t
  (** Human-readable pretty-printer of {!t}. *)
end

module Weighted = struct
  module type R = sig
    (** The type of queues containing elements of type ['a]. *)
    type ('a, 'b) t

    (** Raised when {!push_exn} or {!N.push_exn} is applied to an empty queue. *)
    exception Full

    (** Raised when {!peek_exn}, {!pop_exn} is applied to an empty queue. *)
    exception Empty

    val is_empty : ('a, 'b) t -> bool
    (** Return [true] if the given queue is empty, [false] otherwise. *)

    val create : ?capacity:int -> ('a, 'b) Bigarray_compat.kind -> ('a, 'b) t * int
    (** Return a new queue, initially empty with the real capacity of it. *)

    val length : ('a, 'b) t -> int
    (** Number of elements in the queue. *)

    val available : ('a, 'b) t -> int
    (** Free cells availables on the queue. *)

    val push_exn : ('a, 'b) t -> 'a -> unit
    (** [push_exn q x] adds the elements [x] at the end of the queue [q]. It
       raises {!Full} if the given queue [q] is full. *)

    val push : ('a, 'b) t -> 'a -> unit option
    (** [push q x] is the same as {!push_exn} but returns [None] if it fails. *)

    val pop : ('a, 'b) t -> 'a option
    (** [pop q] removes and returns the first element in the given queue [q]. If
       [q] is empty, it returns [None]. *)

    val pop_exn : ('a, 'b) t -> 'a
    (** [pop_exn q] is the same as {!pop} but it raises an {!Empty} if the given
       queue is empty. *)

    val peek : ('a, 'b) t -> 'a option
    (** [peek q] returns the first element in the given queue [q]. If [q] is
       empty, it returns [None]. *)

    val peek_exn : ('a, 'b) t -> 'a
    (** [peek_exn q] returns the first element in the given queue [q]. If [q] is
       empty, it raises {!Empty}. *)

    val cons_exn : ('a, 'b) t -> 'a -> unit
    (** [cons_exn q x] adds element [x] at the front of the given queue [q]. It
       raises {!Full} if the queue is full. *)

    val cons : ('a, 'b) t -> 'a -> unit option
    (** [cons q x] adds element [x] at the front of the given queue [q]. It
       returns [None] if it fails. *)

    val copy : ('a, 'b) t -> ('a, 'b) t
    (** Return a copy of the given queue. *)

    val clear : ('a, 'b) t -> unit
    (** Discard all elements from a queue. *)

    val compress : ('a, 'b) t -> unit
    (** Compress queue, read cursor will be setted to [0] and data will be move
       to. This operation allows to provide much more space for a
       {!push}/{!N.push} operation - but it can not ensure enough free space. *)

    module N : sig
      (** The type of the internal bigarray of {!t}. *)
      type ('a, 'b) bigarray = ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t

      (** The type of the [blit] function. *)
      type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit

      (** The type of the [length] function. *)
      type 'a length = 'a -> int

      val push_exn :
           ('a, 'b) t
        -> blit:('src, ('a, 'b) bigarray) blit
        -> length:'src length
        -> ?off:int
        -> ?len:int
        -> 'src
        -> ('a, 'b) bigarray list
      (** [push_exn q ~blit ~length ?off ?len src] {i blits} elements in [src]
         to the given queue [q] at the end (like a fast iterative {!R.push}).
         Default value of [off] is [0]. Default value of [len] is [length src - off].
         It returns a list of internal {!bigarray}s which contain [dst].
         If the given [q] does not have enough free space to write [src], it
         raises {!Full} and the given queue is unchanged. *)

      val push :
           ('a, 'b) t
        -> blit:('src, ('a, 'b) bigarray) blit
        -> length:'src length
        -> ?off:int
        -> ?len:int
        -> 'src
        -> ('a, 'b) bigarray list option
      (** Same as {!push_exn} but it returns [None] if it fails. *)

      val keep_exn :
           ('a, 'b) t
        -> blit:(('a, 'b) bigarray, 'dst) blit
        -> length:'dst length
        -> ?off:int
        -> ?len:int
        -> 'dst
        -> unit
      (** [keep_exn q ~blit ~length ?off ?len dst] {i blits} elements of the
         given queue [q] in [dst] from the front to the end of [dst] (like a
         fast iterative {!R.pop_exn}). Default value of [off] is [0]. Default
         value of [len] is [length dst - off]. If the given [q] does not have
         enough elements to write on [dst], it raises {!Empty}. In any case, the
         given queue is unchanged. *)

      val keep :
           ('a, 'b) t
        -> blit:(('a, 'b) bigarray, 'dst) blit
        -> length:'dst length
        -> ?off:int
        -> ?len:int
        -> 'dst
        -> unit option
      (** Same as {!keep_exn} but if it fails, it returns [None]. *)

      val peek : ('a, 'b) t -> ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t list
      (** Returns a sub-part of available to read payloads. *)

      val unsafe_shift : ('a, 'b) t -> int -> unit
      (** [unsafe_shift q l] discards [l] elements in the given queue [q]
         without any verification. Mostly used after {!keep_exn}, if the last
         one does not raise {!Empty}, it's safe to use it. *)

      val shift_exn : ('a, 'b) t -> int -> unit
      (** [shift_exn q l] discards [l] elements in the given queue [q]. If [q]
         does not have enough elements, it raises {!Empty} and the given queue
         is unchanged. *)

      val shift : ('a, 'b) t -> int -> unit option
      (** Same as {!shift_exn} but if it fails, it returns [None]. *)
    end

    val iter : ('a -> unit) -> ('a, 'b) t -> unit
    (** [iter f q] applies [f] in turn to all elements of [q], from the least
       recently entered to the most recently entered. The queue itself is
       unchanged. *)

    val rev_iter : ('a -> unit) -> ('a, 'b) t -> unit
    (** [iter f q] applies [f] in turn to all elements of [q], from the most
       recently entered to the least recently entered. The queue itself is
       unchanged. *)

    val fold : ('acc -> 'x -> 'acc) -> 'acc -> ('x, 'b) t -> 'acc
    (** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the
       list of [q]'s elements. The queue remains unchanged. *)

    val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> ('a, 'b) t Fmt.t
    (** Pretty-printer of {!t}. *)

    val dump : 'a Fmt.t -> ('a, 'b) t Fmt.t
    (** Human-readable pretty-printer of {!t}. *)

    (** / **)

    val from : ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t -> ('a, 'b) t
  end

  module type F = sig
    (** The type of queues containing elements of type ['a]. *)
    type ('a, 'b) t

    (** Raised when {!push_exn} or {!N.push_exn} is applied to an empty queue. *)
    exception Empty

    (** Raised when {!peek_exn}, {!pop_exn} is applied to an empty queue. *)
    exception Full

    val is_empty : ('a, 'b) t -> bool
    (** Return [true] if the given queue is empty, [false] otherwise. *)

    val create : ?capacity:int -> ('a, 'b) Bigarray_compat.kind -> ('a, 'b) t * int
    (** Return a new queue, initially empty with the real capacity of it. *)

    val length : ('a, 'b) t -> int
    (** Number of elements in the queue. *)

    val available : ('a, 'b) t -> int
    (** Free cells availables on the queue. *)

    val push_exn : ('a, 'b) t -> 'a -> ('a, 'b) t
    (** [push_exn q x] adds the elements [x] at the end of the queue [q] and
       returns the new queue [q']. It raises {!Full} if the given queue [q] is
       full. *)

    val push : ('a, 'b) t -> 'a -> ('a, 'b) t option
    (** [push q x] is the same as {!push_exn} but returns [None] if it fails. *)

    val pop : ('a, 'b) t -> ('a * ('a, 'b) t) option
    (** [pop q] removes and returns the first element in the given queue [q] and
       returns the new queue [q']. If [q] is empty, it returns [None]. *)

    val pop_exn : ('a, 'b) t -> 'a * ('a, 'b) t
    (** [pop_exn q] is the same as {!pop} but it raises an {!Empty} if the given
       queue is empty. *)

    val peek : ('a, 'b) t -> 'a option
    (** [peek q] returns the first element in the given queue [q]. If [q] is
       empty, it returns [None]. The given queue [q] is unchanged. *)

    val peek_exn : ('a, 'b) t -> 'a
    (** [peek_exn q] returns the first element in the given queue [q]. If [q] is
       empty, it raises {!Empty}. *)

    val cons : ('a, 'b) t -> 'a -> ('a, 'b) t option
    (** [cons q x] adds element [x] at the front of the given queue [q]. It
       returns [None] if it fails or the new queue [q']. *)

    val cons_exn : ('a, 'b) t -> 'a -> ('a, 'b) t
    (** [cons q x] adds element [x] at the front of the given queue [q]. It
       raises {!Empty} if the given queue [q] is full or the new queue [q']. *)

    val copy : ('a, 'b) t -> ('a, 'b) t
    (** Return a copy of the given queue. *)

    val clear : ('a, 'b) t -> ('a, 'b) t
    (** Discard all elements from a queue. *)

    module N : sig
      (** The type of the internal bigarray of {!t}. *)
      type ('a, 'b) bigarray = ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t

      (** The type of the [blit] function. *)
      type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit

      (** The type of the [length] function. *)
      type 'a length = 'a -> int

      val push_exn :
           ('a, 'b) t
        -> blit:('src, ('a, 'b) bigarray) blit
        -> length:'src length
        -> ?off:int
        -> ?len:int
        -> 'src
        -> ('a, 'b) bigarray list * ('a, 'b) t
      (** [push_exn q ~blit ~length ?off ?len src] {i blits} elements in [src]
         to the given queue [q] at the end (like a fast iterative {!R.push}).
         Default value of [off] is [0]. Default value of [len] is [length src - off].
         It returns a list of internal {!bigarray}s which contain [dst].
         If the given [q] does not have enough free space to write [src], it
         raises {!Full} and the given queue is unchanged. *)

      val push :
           ('a, 'b) t
        -> blit:('src, ('a, 'b) bigarray) blit
        -> length:'src length
        -> ?off:int
        -> ?len:int
        -> 'src
        -> (('a, 'b) bigarray list * ('a, 'b) t) option
      (** Same as {!push_exn} but it returns [None] if it fails. *)

      val keep_exn :
           ('a, 'b) t
        -> blit:(('a, 'b) bigarray, 'dst) blit
        -> length:'dst length
        -> ?off:int
        -> ?len:int
        -> 'dst
        -> unit
      (** [keep_exn q ~blit ~length ?off ?len dst] {i blits} elements of the
         given queue [q] in [dst] from the front to the end of [dst] (like a
         fast iterative {!R.pop_exn}). Default value of [off] is [0]. Default
         value of [len] is [length dst - off]. If the given [q] does not have
         enough elements to write on [dst], it raises {!Empty}. In any case, the
         given queue is unchanged. *)

      val keep :
           ('a, 'b) t
        -> blit:(('a, 'b) bigarray, 'dst) blit
        -> length:'dst length
        -> ?off:int
        -> ?len:int
        -> 'dst
        -> unit option
      (** Same as {!keep_exn} but if it fails, it returns [None]. *)

      val unsafe_shift : ('a, 'b) t -> int -> ('a, 'b) t
      (** [unsafe_shift q l] discards [l] elements in the given queue [q]
         without any verification. Mostly used after {!keep_exn}, if the last
         one does not raise {!Empty}, it's safe to use it. *)

      val shift_exn : ('a, 'b) t -> int -> ('a, 'b) t
      (** [shift_exn q l] discards [l] elements in the given queue [q]. If [q]
         does not have enough elements, it raises {!Empty} and the given queue
         is unchanged. *)

      val shift : ('a, 'b) t -> int -> ('a, 'b) t option
      (** Same as {!shift_exn} but if it fails, it returns [None]. *)
    end

    val iter : ('a -> unit) -> ('a, 'b) t -> unit
    (** [iter f q] applies [f] in turn to all elements of [q], from the least
       recently entered to the most recently entered. The queue itself is
       unchanged. *)

    val rev_iter : ('a -> unit) -> ('a, 'b) t -> unit
    (** [iter f q] applies [f] in turn to all elements of [q], from the most
       recently entered to the least recently entered. The queue itself is
       unchanged. *)

    val fold : ('acc -> 'x -> 'acc) -> 'acc -> ('x, 'b) t -> 'acc
    (** [fold f a q] is equivalent to [List.fold_left f a l], where [l] is the
       list of [q]'s elements. The queue remains unchanged. *)

    val pp : ?sep:unit Fmt.t -> 'a Fmt.t -> ('a, 'b) t Fmt.t
    (** Pretty-printer of {!t}. *)

    val dump : 'a Fmt.t -> ('a, 'b) t Fmt.t
    (** Human-readable pretty-printer of {!t}. *)

    (** / **)

    val unsafe_bigarray :
      ('a, 'b) t -> ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t

    val from : ('a, 'b, Bigarray_compat.c_layout) Bigarray_compat.Array1.t -> ('a, 'b) t
  end
end
