open Base

module Configure (Scalar : Scalar.S) (Dimension : Dimension.S) = struct
  module type Entity = sig
    type t

    val max : t -> Dimension.t -> Scalar.t

    val min : t -> Dimension.t -> Scalar.t

    val to_string : t -> string
  end

  module Create (Entity : Entity) = struct
    let _average items =
      let open Scalar in
      let total, count =
        List.fold_left items ~init:(Scalar.zero, Scalar.zero)
          ~f:(fun (total, count) next -> (total + next, count + Scalar.one))
      in
      total / count

    type node = {
      location : Scalar.t * Scalar.t;
      dim : Dimension.t;
      left : t;
      right : t;
    }

    and leaf = Entity.t list

    and t = Node of node | Leaf of leaf

    let to_string (t : t) : string =
      let rec _to_string (t : t) (depth : int) : string =
        match t with
        | Leaf [] -> Printf.sprintf "%*s-leaf *" depth " "
        | Leaf leaves ->
            List.fold leaves ~init:"" ~f:(fun prev leaf ->
                Printf.sprintf "%s%*sleaf %s" prev depth " "
                  (Entity.to_string leaf))
        | Node node ->
            let min, max = node.location in
            Printf.sprintf "%*sdim %s on %s %s \n%s\n%s" depth " "
              (Dimension.to_string node.dim)
              (Scalar.to_string min) (Scalar.to_string max)
              (_to_string node.left (depth + 2))
              (_to_string node.right (depth + 2))
      in
      _to_string t 0

    let _boundary (e : Entity.t) (dim : Dimension.t) =
      (Entity.min e dim, Entity.max e dim)

    let rec _split_compare left right remaining ~anchor ~dim =
      let open Scalar in
      match remaining with
      | [] -> (left, right)
      | hd :: tl when compare (Entity.max hd dim) anchor < 0 ->
          _split_compare (hd :: left) right tl ~anchor ~dim
      | hd :: tl when compare (Entity.min hd dim) anchor < 0 ->
          _split_compare (hd :: left) (hd :: right) tl ~anchor ~dim
      | hd :: tl -> _split_compare left (hd :: right) tl ~anchor ~dim

    let rec _create (entities : Entity.t list)
        (dimensions : Dimension.t list) : t =
      let entity_count = List.length entities in
      match (entities, dimensions) with
      | _, [] -> Leaf entities
      | [], _ -> Leaf []
      | [ hd ], _ -> Leaf [ hd ]
      | _, dim :: other_dimensions -> (
          let a0, a1, a2 =
            match entities with
            | [ hd0; hd1 ] -> (hd0, hd0, hd1)
            | [ hd0; hd1; hd2 ] -> (hd0, hd1, hd2)
            | _ ->
                let a0 = List.random_element_exn entities in
                let a1 = List.random_element_exn entities in
                let a2 = List.random_element_exn entities in
                (a0, a1, a2)
          in
          (* instead of sorting and using the median, sample a few random
             points and then divide on it *)
          let a0_min, a0_max = _boundary a0 dim in
          let a1_min, a1_max = _boundary a1 dim in
          let a2_min, a2_max = _boundary a2 dim in
          let anchor_min = _average [ a0_min; a1_min; a2_min ] in
          let anchor_max = _average [ a0_max; a1_max; a2_max ] in
          match _split_compare [] [] entities ~anchor:anchor_max ~dim with
          | left, [] -> Leaf left
          | [], right -> Leaf right
          (* if this dimension failed to do any meaningful work, exclude it
             from future constructions *)
          | left, right
            when List.length left = entity_count
                 || List.length right = entity_count ->
              _create entities other_dimensions
          | left, right ->
              let dimensions = other_dimensions @ [ dim ] in
              Node
                {
                  location = (anchor_min, anchor_max);
                  dim;
                  left = _create left dimensions;
                  right = _create right dimensions;
                } )

    let rec _intersecting (t : t) (entity : Entity.t) : Entity.t list =
      match t with
      | Leaf e -> e
      | Node node ->
          let e_min, e_max = _boundary entity node.dim in
          let _, anchor_max = node.location in
          if Scalar.compare e_min anchor_max > 0 then
            _intersecting node.right entity
          else if Scalar.compare e_max anchor_max < 0 then
            _intersecting node.left entity
          else
            _intersecting node.left entity @ _intersecting node.right entity

    let create (entities : Entity.t list) : t =
      _create entities Dimension.dimensions

    let rec traverse (t : t) ~n =
      match t with
      | Leaf e -> List.map e ~f:n
      | Node node -> traverse node.left ~n @ traverse node.right ~n

    let _intersect (e0 : Entity.t) (e1 : Entity.t) =
      List.fold_left Dimension.dimensions ~init:true ~f:(fun prev dim ->
          let s0, s1 = _boundary e0 dim in
          let s2, s3 = _boundary e1 dim in
          prev && not (Scalar.compare s0 s3 > 0 || Scalar.compare s2 s1 > 0))

    let intersecting (t : t) (entity : Entity.t) : Entity.t list =
      List.filter (_intersecting t entity) ~f:(fun e -> _intersect e entity)
  end
end

let%test_module _ =
  ( module struct
    module Dimension = Dimension.One

    module Entity = struct
      type t = { value : string; x : int; w : int }

      let min (t : t) (_ : Dimension.t) = t.x

      let max (t : t) (_ : Dimension.t) = t.x + t.w

      let to_string (t : t) = Printf.sprintf "%s %d %d" t.value t.x t.w
    end

    module Kd_tree_builder = Configure (Int) (Dimension)
    module Kd_tree = Kd_tree_builder.Create (Entity)

    let dim = ()

    let%test_unit "Create three branches" =
      let entities : Entity.t list =
        [
          { value = "left"; x = -3; w = 3 };
          { value = "right"; x = 7; w = 3 };
          { value = "center"; x = 5; w = 3 };
        ]
      in
      let tree =
        Kd_tree.traverse (Kd_tree.create entities) ~n:(fun e -> e.value)
      in
      [%test_result: string list] tree
        ~expect:[ "left"; "center"; "center"; "right" ]

    let%test_unit "Assert correct ordering" =
      let entities : Entity.t list =
        [
          { value = "4th"; x = 6; w = 1 }; { value = "3rd"; x = 4; w = 1 };
          { value = "1st"; x = 0; w = 1 }; { value = "5th"; x = 8; w = 1 };
          { value = "2nd"; x = 2; w = 1 };
        ]
      in
      let tree =
        Kd_tree.traverse (Kd_tree.create entities) ~n:(fun e -> e.value)
      in
      [%test_result: string list] tree
        ~expect:[ "1st"; "2nd"; "3rd"; "4th"; "4th"; "5th" ]

    let%test_unit "Verify split on anchor" =
      let anchor = 5 in
      let entities : Entity.t list =
        [
          { value = "left 2"; x = 4; w = 2 };
          { value = "left 3"; x = 5; w = 2 };
          { value = "right 4"; x = 6; w = 2 };
          { value = "left 0"; x = 1; w = 2 };
          { value = "left 1"; x = 3; w = 2 };
        ]
      in
      let left, right = Kd_tree._split_compare [] [] entities ~anchor ~dim in
      let left = List.map left ~f:(fun e -> e.value) in
      let right = List.map right ~f:(fun e -> e.value) in
      [%test_result: string list * string list] (left, right)
        ~expect:
          ( [ "left 1"; "left 0"; "left 2" ],
            [ "left 1"; "right 4"; "left 3"; "left 2" ] )

    let%test_unit "Follow branch right and find intersect" =
      let open Kd_tree in
      let node =
        Node
          {
            location = (-5, 3);
            dim = ();
            left =
              Node
                {
                  location = (-5, 3);
                  dim = ();
                  left = Leaf [ { value = "left"; x = -5; w = 3 } ];
                  right = Leaf [ { value = "center"; x = 0; w = 3 } ];
                };
            right = Leaf [ { value = "right"; x = 4; w = 3 } ];
          }
      in
      let hit : Entity.t = { value = "test"; x = 5; w = 3 } in
      let results =
        List.map (Kd_tree._intersecting node hit) ~f:(fun e -> e.value)
      in
      [%test_result: string list] results ~expect:[ "right" ]

    let%test_unit "Follow branch left and find intersect" =
      let open Kd_tree in
      let node =
        Node
          {
            location = (-5, 3);
            dim = ();
            left =
              Node
                {
                  location = (-5, 0);
                  dim = ();
                  left = Leaf [ { value = "left"; x = -5; w = 3 } ];
                  right = Leaf [ { value = "center"; x = 0; w = 3 } ];
                };
            right = Leaf [ { value = "right"; x = 4; w = 3 } ];
          }
      in
      let hit : Entity.t = { value = "test"; x = -4; w = 3 } in
      let results =
        List.map (Kd_tree._intersecting node hit) ~f:(fun e -> e.value)
      in
      [%test_result: string list] results ~expect:[ "left" ]

    let%test_unit "Follow branch right, include overlap, and find intersect"
        =
      let open Kd_tree in
      let node =
        Node
          {
            location = (-5, 3);
            dim = ();
            left =
              Node
                {
                  location = (-5, 0);
                  dim = ();
                  left = Leaf [ { value = "left"; x = -5; w = 3 } ];
                  right = Leaf [ { value = "center"; x = 0; w = 3 } ];
                };
            right = Leaf [ { value = "right"; x = 4; w = 3 } ];
          }
      in
      let hit : Entity.t = { value = "test"; x = 3; w = 4 } in
      let results =
        List.map (Kd_tree._intersecting node hit) ~f:(fun e -> e.value)
      in
      [%test_result: string list] results ~expect:[ "center"; "right" ]

    let%test_unit "Follow branch left, include overlap, and find intersect" =
      let open Kd_tree in
      let node =
        Node
          {
            location = (-5, 3);
            dim = ();
            left =
              Node
                {
                  location = (-5, 0);
                  dim = ();
                  left = Leaf [ { value = "left"; x = -5; w = 3 } ];
                  right = Leaf [ { value = "center"; x = 0; w = 3 } ];
                };
            right = Leaf [ { value = "right"; x = 4; w = 3 } ];
          }
      in
      let hit : Entity.t = { value = "test"; x = -4; w = 5 } in
      let results =
        List.map (Kd_tree._intersecting node hit) ~f:(fun e -> e.value)
      in
      [%test_result: string list] results ~expect:[ "left"; "center" ]

    let%test_unit "Include all branches" =
      let open Kd_tree in
      let node =
        Node
          {
            location = (-5, 3);
            dim = ();
            left =
              Node
                {
                  location = (-5, 0);
                  dim = ();
                  left = Leaf [ { value = "left"; x = -5; w = 3 } ];
                  right = Leaf [ { value = "center"; x = 0; w = 3 } ];
                };
            right = Leaf [ { value = "right"; x = 4; w = 3 } ];
          }
      in
      let hit : Entity.t = { value = "test"; x = -20; w = 40 } in
      let results =
        List.map (Kd_tree._intersecting node hit) ~f:(fun e -> e.value)
      in
      [%test_result: string list] results
        ~expect:[ "left"; "center"; "right" ]
  end )

let%test_module _ =
  ( module struct
    module Dimension = Dimension.Two

    module Entity = struct
      type t = { value : string; x : float; y : float; w : float; h : float }

      let min (t : t) (dimension : Dimension.t) =
        match dimension with X -> t.x | Y -> t.y

      let max (t : t) (dimension : Dimension.t) =
        match dimension with X -> t.x +. t.w | Y -> t.y +. t.h

      let to_string (t : t) =
        Printf.sprintf "%s %f,%f,%f,%f" t.value t.x t.y t.w t.h
    end

    module Kd_tree_builder = Configure (Float) (Dimension)
    module Kd_tree = Kd_tree_builder.Create (Entity)

    let%test_unit "Intersect two things" =
      let t =
        Kd_tree.create
          [
            { value = "red"; x = 1.0; y = 1.0; w = 10.0; h = 10.0 };
            { value = "blu"; x = 150.0; y = 1.0; w = 10.0; h = 10.0 };
            { value = "ora"; x = -50.0; y = -50.0; w = 100.0; h = 100.0 };
          ]
      in
      let hit =
        List.map
          (Kd_tree.intersecting t
             { value = ""; x = 0.0; y = 0.0; w = 2.0; h = 2.0 })
          ~f:(fun e -> e.value)
      in
      [%test_result: string list] hit ~expect:[ "ora"; "red" ]

    let%test_unit "Intersect one thing" =
      let t =
        Kd_tree.create
          [
            { value = "red"; x = 0.0; y = 0.0; w = 10.0; h = 10.0 };
            { value = "blu"; x = 50.0; y = 0.0; w = 10.0; h = 10.0 };
            { value = "ora"; x = -50.0; y = -50.0; w = 100.0; h = 100.0 };
          ]
      in
      let hit =
        List.map
          (Kd_tree.intersecting t
             { value = ""; x = 12.0; y = 9.0; w = 2.0; h = 2.0 })
          ~f:(fun e -> e.value)
      in
      [%test_result: string list] hit ~expect:[ "ora" ]
  end )
