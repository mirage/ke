type 'a t

val is_empty : 'a t -> bool
val empty : 'a t
val push : 'a t -> 'a -> 'a t
val shift : 'a t -> 'a * 'a t
val cons : 'a t -> 'a -> 'a t
