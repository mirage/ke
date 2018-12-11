type t

val create : ?capacity:int -> unit -> t
val push : t -> char -> unit
val shift : t -> char
val cons : t -> char -> unit

module N : sig
  type ('a, 'b) blit = 'a -> int -> 'b -> int -> int -> unit
  type 'a length = 'a -> int

  val push :
    t -> blit:('a, Bigstringaf.t) blit -> length:'a length -> ?off:int -> ?len:int -> 'a -> unit

  val keep :
    t -> blit:(Bigstringaf.t, 'a) blit -> length:'a length -> ?off:int -> ?len:int -> 'a -> unit

  val shift : t -> int -> unit
end
