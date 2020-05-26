open Base
module Dimension = Kd_tree.Dimension.Two

module Entity = struct
  type t = {
    color : string;
    x : float;
    y : float;
    s : float;
    time : float;
    speed : float;
    radius : float;
    px0 : float;
    px1 : float;
    py0 : float;
    py1 : float;
  }

  let to_string t = t.color

  let set_time (t : t) ~(time : float) =
    let rel_time = t.speed *. time in
    let x = t.x +. (Float.cos rel_time *. t.radius) -. (t.s *. 0.5) in
    let y = t.y +. (Float.sin rel_time *. t.radius) -. (t.s *. 0.5) in
    { t with time; px0 = x; px1 = x +. t.s; py0 = y; py1 = y +. t.s }

  let min (t : t) (dimension : Dimension.t) =
    match dimension with X -> t.px0 | Y -> t.py0

  let max (t : t) (dimension : Dimension.t) =
    match dimension with X -> t.px1 | Y -> t.py1
end

module Kd_tree = Kd_tree.Float_2d.Create (Entity)

module Render = struct
  open Js_of_ocaml

  let run () =
    let document = Dom_html.window##.document in
    let canvas = Dom_html.createCanvas document in
    let width = 600 in
    let height = 600 in
    canvas##.width := width;
    canvas##.height := height;
    Dom.appendChild document##.body canvas;
    let context = canvas##getContext Dom_html._2d_ in

    let outline_rect x y w h ~color =
      context##.lineWidth := 1.;
      context##.strokeStyle := Js.string color;
      context##beginPath;
      context##rect x y w h;
      context##closePath;
      context##stroke;
      ()
    in

    let fill_rect x y w h ~color =
      context##.lineWidth := 1.;
      context##.fillStyle := Js.string color;
      context##beginPath;
      context##rect x y w h;
      context##closePath;
      context##fill;
      ()
    in

    let tree = ref (Kd_tree.create []) in
    let intersect : Entity.t list ref = ref [] in
    let entities : Entity.t list ref = ref [] in
    let time = ref 20. in

    let cursor =
      ref
        {
          Entity.color = "#F00";
          x = 0.;
          y = 0.;
          s = 2.0;
          time = 0.0;
          speed = 0.0;
          radius = 0.0;
          px0 = 0.;
          py0 = 0.;
          px1 = 0.;
          py1 = 0.;
        }
    in

    let rec render_tree (tree : Kd_tree.t) (depth : int) =
      match tree with
      | Leaf entities ->
          List.iter entities ~f:(fun e ->
              let x0, x1 =
                (Entity.min e Dimension.X, Entity.max e Dimension.X)
              in
              let y0, y1 =
                (Entity.min e Dimension.Y, Entity.max e Dimension.Y)
              in
              outline_rect x0 y0 (x1 -. x0) (y1 -. y0) ~color:"#888";
              ())
      | Node node ->
          render_tree node.left (depth + 3);
          render_tree node.right (depth + 3);
          (* let _, _ = node.location in match node.dim with | Dimension.X ->
             render_rect v1 0. 0. 2000. ~color:(Printf.sprintf "#%d00" depth)
             | Dimension.Y -> render_rect 0. v1 2000. 0.
             ~color:(Printf.sprintf "#0%d0" depth) *)
          ()
    in

    let render_entity (e : Entity.t) =
      let x0, x1 = (Entity.min e Dimension.X, Entity.max e Dimension.X) in
      let y0, y1 = (Entity.min e Dimension.Y, Entity.max e Dimension.Y) in
      fill_rect x0 y0 (x1 -. x0) (y1 -. y0) ~color:"#5f5"
    in

    let step () =
      time := !time +. 0.001;
      entities := List.map !entities ~f:(Entity.set_time ~time:!time);
      tree := Kd_tree.create !entities;
      intersect := Kd_tree.intersecting !tree !cursor;
      ()
    in

    let rec render () =
      step ();
      context##clearRect 0. 0. (Float.of_int width) (Float.of_int height);
      render_tree !tree 0;
      let _ = List.map !intersect ~f:render_entity in
      Dom_html._requestAnimationFrame
        (Js.wrap_callback (fun () -> render ()));
      ()
    in

    let update_cursor x y =
      cursor :=
        Entity.set_time
          { !cursor with x = Float.of_int x; y = Float.of_int y }
          ~time:0.;
      ()
    in

    let insert_entity x y =
      let entity : Entity.t =
        let radius = Random.float 500. -. 1.0 in
        {
          x = Float.of_int x;
          y = Float.of_int y;
          s = (Random.float 100. *. radius *. 0.001) +. 2.;
          radius;
          speed =
            (((Random.float 50. +. 2.) *. radius *. 0.001) -. 5.) /. 10.;
          time = 0.;
          color = "#444";
          px0 = 0.;
          py0 = 0.;
          px1 = 0.;
          py1 = 0.;
        }
      in
      entities := entity :: !entities;
      ()
    in

    canvas##.onmousemove :=
      Dom_html.handler (fun ev ->
          update_cursor ev##.clientX ev##.clientY;
          Js._false);

    List.iter (List.range 0 2000) ~f:(fun _ ->
        insert_entity (width / 2) (height / 2));

    render ();

    ()

  let _ =
    Dom_html.window##.onload :=
      Dom_html.handler (fun _ ->
          run ();
          Js._true)
end
