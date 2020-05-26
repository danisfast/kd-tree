module Configure : functor
  (Scalar : Scalar.S)
  (Dimension : Dimension.S)
  -> sig
  module type Entity = sig
    type t

    val max : t -> Dimension.t -> Scalar.t

    val min : t -> Dimension.t -> Scalar.t

    val to_string : t -> string
  end

  module Create : functor (Entity : Entity) -> sig
    type node = {
      location : Scalar.t * Scalar.t;
      dim : Dimension.t;
      left : t;
      right : t;
    }

    and leaf = Entity.t list

    and t = Node of node | Leaf of leaf

    val to_string : t -> string

    val create : Entity.t list -> t

    val intersecting : t -> Entity.t -> Entity.t list
  end
end
