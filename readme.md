![What it looks like](/kd_tree.gif?raw=true "What it looks like")

#### About

An ocaml volume based kd-tree implementation.

[src/kd_tree](https://github.com/danisfast/kd-tree/blob/master/src/kd_tree/kd_tree.ml) shares a few helpful modules
- `Int_2d`
- `Int_3d`
- `Float_2d`
- `Float_3d`

These can be instantiated as a `Kd_tree` by specifying an Entity module.

#### Example

In the case of `Int_2d`, the Entity module would have the following signature...

```ocaml
module type Entity = sig
    type t
    val max : t -> Dimension.t -> int
    val min : t -> Dimension.t -> int
    val to_string : t -> string
end
```

- `Dimension.t` is either `X` or `Y`
- `min` returns the smaller edge of the dimension
- `max` returns the larger edge of the dimension
- `to_string` is helpful for debugging

An example implementation can be found at [src/example/main.ml](https://github.com/danisfast/kd-tree/blob/master/src/example/main.ml)

#### Caveats

- This implementation determines the node split by sampling randomly (rather than sorting)
- Entities that cross a boundary can show up on both sides of the tree following a split
- Duplicate results can be returned from an intersect query

#### Commands

- `make bench` - Build and run local benchmarks
- `make test`  - Build and run unit tests
- `make ui`    - Build and run a sample JS application (the one in the GIF) with a `python -m SimpleHttpServer`
