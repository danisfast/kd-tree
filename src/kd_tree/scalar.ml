module type S = sig
  type t

  val one : t

  val zero : t

  val ( + ) : t -> t -> t

  val ( / ) : t -> t -> t

  val compare : t -> t -> int

  val to_string : t -> string
end
