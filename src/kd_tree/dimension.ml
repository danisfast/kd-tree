module type S = sig
  type t

  val dimensions : t list

  val to_string : t -> string
end

module One = struct
  type t = unit

  let dimensions = [ () ]

  let to_string _ = "unit"
end

module Two = struct
  type t = X | Y

  let dimensions = [ X; Y ]

  let to_string = function X -> "x" | Y -> "y"
end

module Three = struct
  type t = X | Y | Z

  let dimensions = [ X; Y; Z ]

  let to_string = function X -> "x" | Y -> "y" | Z -> "z"
end
