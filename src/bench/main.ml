open Core
open Core_bench

module type Test_config = sig
  type t

  val random : t -> t

  val range : t

  val scale : t
end

module Create (S : Kd_tree.Scalar.S) (C : Test_config with type t = S.t) =
struct
  open Kd_tree
  module Kd_entity_tree_builder =
    Kd_tree.Builder.Configure (S) (Dimension.Two)

  module Entity = struct
    type t = { x : S.t; w : S.t; y : S.t; h : S.t }

    let min (t : t) (dim : Dimension.Two.t) =
      match dim with X -> t.x | Y -> t.y

    let max (t : t) (dim : Dimension.Two.t) =
      let open S in
      match dim with X -> t.x + t.w | Y -> t.y + t.h

    let to_string (_ : t) = ""
  end

  module Kd_entity_tree = Kd_entity_tree_builder.Create (Entity)

  let measure_building ~square_counts ~label =
    Bench.Test.create_indexed ~args:square_counts
      ~name:(Printf.sprintf "Create %s" label) (fun count ->
        let entities =
          List.map (List.range 0 count) ~f:(fun _ ->
              {
                Entity.x = C.random C.range;
                y = C.random C.range;
                w = C.scale;
                h = C.scale;
              })
        in
        Staged.stage (fun _ -> Kd_entity_tree.create entities))

  let measure_intersecting ~square_counts ~intersection_count ~label =
    Bench.Test.create_indexed ~args:square_counts
      ~name:(Printf.sprintf "Intersect %s" label) (fun count ->
        let entities =
          List.map (List.range 0 count) ~f:(fun _ ->
              {
                Entity.x = C.random C.range;
                y = C.random C.range;
                w = C.scale;
                h = C.scale;
              })
        in
        let tree = Kd_entity_tree.create entities in
        let intersects =
          List.map (List.range 0 intersection_count) ~f:(fun _ ->
              {
                Entity.x = C.random C.range;
                y = C.random C.range;
                w = S.zero;
                h = S.zero;
              })
        in
        Staged.stage (fun _ ->
            List.map intersects ~f:(fun intersect ->
                Kd_entity_tree.intersecting tree intersect)))
end

module Int_test =
  Create
    (Int)
    (struct
      type t = Int.t

      let random v = Random.int v

      let range = 10000

      let scale = 10
    end)

module Float_test =
  Create
    (Float)
    (struct
      type t = Float.t

      let random v = Random.float v

      let range = 10000.

      let scale = 10.
    end)

let command =
  let square_counts = [ 1000; 10000; 100000 ] in
  let intersection_count = 100 in
  Bench.make_command
    [
      Int_test.measure_building ~square_counts ~label:"Int";
      Float_test.measure_building ~square_counts ~label:"Float";
      Int_test.measure_intersecting ~square_counts ~intersection_count
        ~label:"Int";
      Float_test.measure_intersecting ~square_counts ~intersection_count
        ~label:"Float";
    ]

let main () =
  Command.run
    (Command.group ~summary:"Benchmarks" [ ("benchmark", command) ])

let () = main ()
