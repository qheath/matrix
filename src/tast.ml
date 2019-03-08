open Lwt

let () = Random.self_init ()

module Colour : sig

  type t

  val make : unit -> t
  val update : density:float -> t -> t
  val to_attr : t -> Notty.A.t

end = struct

  type t = float option

  let make () = None

  let update ~density colour =
    if Random.float 1. < density
    then Some 1.
    else match colour with
      | Some level ->
        let level = level *. (0.85 +. Random.float 0.15) in
        if level>0.01 then Some level else None
      | None -> None

  let to_attr = function
    | None -> Notty.A.fg Notty.A.lightblack
    | Some level ->
      let point0,level0 = Gg.V3.v 0. 0. 0., 0.
      and pcs = [
        Gg.V3.v 0. 1. 0., 0.3 ;
        Gg.V3.v 1. 1. 1., 1. ;
      ] in
      let r,g,b =
        let point =
          let rec aux (point0,level0) = function
            | [] -> point0
            | (point1,level1 as pc1)::l ->
              if level<level1 then Gg.V3.mix point0 point1 ((level-.level0)/.(level1-.level0))
              else aux pc1 l
          in
          if level<level0 then point0
          else aux (point0,level0) pcs
        in
        Gg.V3.to_tuple point
      and cap x =
        let y = int_of_float @@ x *. 256. in
        if y<0 then 0
        else if y>255 then 255
        else y
      in
      Notty.A.fg @@ Notty.A.rgb_888 ~r:(cap r) ~g:(cap g) ~b:(cap b)

end

module Code : sig

  type t

  val pick : unit -> t
  val to_uchar : t -> Uchar.t

end = struct

  type t = int

  let code_of_utf8 utf8 =
    let uchar =
      match Uutf.decode (Uutf.decoder (`String utf8)) with
      | `Uchar c -> c
      | _ -> assert false
    in
    Uchar.to_int uchar

  let default = code_of_utf8 "　"

  let pick =
    let module Codes =
      Map.Make(struct type t = int let compare = compare end)
    in
    let map,_ =
      let rec add map key code n =
        if n>0 then
          add (Codes.add key code map) (key+1) (code+1) (n-1)
        else map,key
      in
      let aux (map,sum) (low_utf8,high_utf8) =
        let low_code = code_of_utf8 low_utf8
        and high_code = code_of_utf8 high_utf8 in
        add map sum low_code (high_code - low_code + 1)
      in
      List.fold_left aux (Codes.empty,0) [
        "０","９" ;
        "Ａ","Ｚ" ;
        "ａ","ｚ" ;
        "ぁ","ゖ" ;
        "ァ","ヺ" ;
      ]
    in
    fun () ->
      match Codes.find_opt (Random.int (Codes.cardinal map)) map with
      | None -> default
      | Some c -> c

  let to_uchar code =
    Uchar.of_int code

end

module Cell : sig

  type t

  val make : unit -> t
  val update : density:float -> t -> t
  val merge : t -> t -> t
  val to_image : t -> Notty.I.t

end = struct

  type t = Code.t * Colour.t

  let make () = Code.pick (),Colour.make ()

  let update ~density (code,colour) =
    code,(Colour.update ~density colour)

  let merge (code,_) (_,colour) = code,colour

  let to_image (code,colour) =
    Notty.I.uchar (Colour.to_attr colour) (Code.to_uchar code) 1 1

end

let iteri length f a =
  let rec aux i =
    if i>=0 then begin
      f i a.(i) ;
      aux (i-1)
    end
  in
  aux (length-1)

let expand old_length new_length f a =
  if new_length < old_length then begin
    for i=new_length to old_length-1 do
      a.(i) <- f ()
    done ;
    a
  end else if new_length > Array.length a then begin
    let aux i =
      if i < Array.length a then a.(i) else f ()
    in
    Array.init new_length aux
  end else a

module Line : sig

  type t

  val make : w:int -> t
  val update : w:int -> wind_speed:float -> density:float -> t -> unit
  val merge : w:int -> t -> t -> unit
  val to_image : t -> int -> Notty.I.t
  val adapt : w0:int -> w1:int -> t -> t

end = struct

  type t = Cell.t array

  let make ~w =
    Array.init w (fun _ -> Cell.make ())

  let update ~w ~wind_speed ~density line =
    let rand = Random.float 1. in
    ignore rand ;
    ignore wind_speed ;
    let temp = make ~w in
    let aux j cell = temp.(j) <- Cell.update ~density cell in
    iteri w aux line ;
    let aux =
      if rand<wind_speed then
        (fun j cell ->
           let j = if j>0 then j-1 else w-1 in
           line.(j) <- Cell.update ~density cell)
      else if rand<wind_speed*.2. then
        (fun j cell ->
           let j = if j<w-1 then j+1 else 0 in
           line.(j) <- Cell.update ~density cell)
      else
        (fun j cell ->
           line.(j) <- Cell.update ~density cell)
    in
    Array.iteri aux temp

  let merge ~w line_code line_colour =
    let aux j cell =
      line_code.(j) <- Cell.merge line_code.(j) cell
    in
    iteri w aux line_colour

  let to_image line j =
    Cell.to_image line.(j)

  let adapt ~w0 ~w1 line =
    expand w0 w1 Cell.make line

end

module Matrix : sig

  type t

  val make : h:int -> w:int -> t
  val step : w:int -> h:int -> wind_speed:float -> density:float -> t -> unit
  val to_image : w:int -> h:int -> t -> Notty.I.t
  val adapt : w0:int -> h0:int -> w1:int -> h1:int -> t -> t

end = struct

  type t = Line.t array

  let make ~h ~w =
    Array.init h (fun _ -> Line.make ~w)

  let step ~w ~h ~wind_speed ~density lines =
    let aux i line =
      if i>0
      then Line.merge ~w line lines.(i-1)
      else Line.update ~w ~wind_speed ~density line
    in
    iteri h aux lines

  let to_image ~w ~h lines =
    let aux j i = Line.to_image lines.(i) j in
    Notty.I.tabulate w h aux

  let adapt ~w0 ~h0 ~w1 ~h1 lines =
    Array.map (Line.adapt ~w0 ~w1) lines
    |> expand h0 h1 (fun () -> Line.make ~w:w1)

end

module type Counter = sig

  type t

  val make : ?lambda:float -> float -> t
  val incr : t -> t
  val decr : t -> t
  val get : t -> float

end

module BoundedCounter : Counter = struct

  type t = {
    fold : float -> float ;
    unfolded_value : float ;
    folded_value : float ;
  }

  let make ?(lambda=1.) folded_value = {
    fold = (fun x -> 1. /. (1. +. exp (-. lambda *. x))) ;
    unfolded_value = -. log (1. /. folded_value -. 1.) /. lambda ;
    folded_value ;
  }

  let incr {fold;unfolded_value;_} =
    let unfolded_value = unfolded_value +. 1. in
    let folded_value = fold unfolded_value in
    { fold ; unfolded_value ; folded_value }

  let decr {fold;unfolded_value;_} =
    let unfolded_value = unfolded_value -. 1. in
    let folded_value = fold unfolded_value in
    { fold ; unfolded_value ; folded_value }

  let get {folded_value;_} = folded_value

end

module UnboundedCounter : Counter = struct

  type t = {
    lambda : float ;
    value : float ;
  }

  let make ?(lambda=2.) value =
    { lambda ; value }

  let incr {lambda;value} =
    { lambda ; value = value *. lambda }

  let decr {lambda;value} =
    { lambda ; value = value /. lambda }

  let get {value;_} = value

end

module Event : sig

  type t
  type stream
  type action = [
    | `PageUp
    | `PageDown
    | `Up
    | `Down
    | `Left
    | `Right
    | `Quit
    | `Resize of int * int
    | `Ignore
  ]

  val stream_of_term : Notty_lwt.Term.t -> stream
  val pop : stream -> (t * stream) option
  val interpret : t -> action

end = struct

  type t = [
    | `Key of Notty.Unescape.key
    | `Mouse of Notty.Unescape.mouse
    | `Paste of Notty.Unescape.paste
    | `Resize of int * int
  ]

  type stream = t list * t Lwt_stream.t

  type action = [
    | `PageUp
    | `PageDown
    | `Up
    | `Down
    | `Left
    | `Right
    | `Quit
    | `Resize of int * int
    | `Ignore
  ]

  let stream_of_term term =
    [],Notty_lwt.Term.events term

  let pop (event_list,event_stream) =
    match
      match event_list with
      | event::event_list -> Some (event,event_list)
      | [] ->
        match Lwt_stream.get_available event_stream with
        | event::event_list -> Some (event,event_list)
        | [] -> None
    with
    | None -> None
    | Some (event,event_list) ->
      Some (event,(event_list,event_stream))

  let interpret = function
    | `Key key ->
      begin match key with
        | `Page page,_ ->
          begin match page with
            | `Up -> `PageUp
            | `Down -> `PageDown
          end
        | `Arrow arrow,_ ->
          begin match arrow with
            | `Up -> `Up
            | `Down -> `Down
            | `Left -> `Left
            | `Right -> `Right
          end
        | _ -> `Quit
      end
    | `Resize (w,h) -> `Resize (w,h)
    | `Paste _ | `Mouse _ -> `Ignore

end

module Status : sig

  type t

  val init : unit -> t Lwt.t
  val display : t -> unit Lwt.t
  val update : t -> t option
  val close : t -> unit Lwt.t

end = struct

  type t = {
    term : Notty_lwt.Term.t ;
    events : Event.stream ;
    w : int ;
    h : int ;
    matrix : Matrix.t ;
    wind_speed : UnboundedCounter.t ;
    frame_rate : UnboundedCounter.t ;
    density : BoundedCounter.t ;
    ts : int ;
  }

  let init () =
    let term = Notty_lwt.Term.create () in
    let events = Event.stream_of_term term in
    let w_term,h_term = Notty_lwt.Term.size term in
    let w,h = w_term/2,h_term in
    let matrix = Matrix.make ~w ~h in
    let wind_speed = UnboundedCounter.make ~lambda:1. 0.015
    and frame_rate = UnboundedCounter.make ~lambda:2. 0.05
    and density = BoundedCounter.make ~lambda:0.3 0.01
    and ts = 0 in
    Lwt.return {
      term ;
      events ;
      w ;
      h ;
      matrix ;
      frame_rate ;
      wind_speed ;
      density ;
      ts ;
    }

  let display {term;w;h;matrix;frame_rate;_} =
    Matrix.to_image ~w ~h matrix |> Notty_lwt.Term.image term ;%lwt
    Lwt_unix.sleep @@ UnboundedCounter.get frame_rate

  let end_ts = max_int

  let update status =
    Matrix.step ~w:status.w ~h:status.h
      ~wind_speed:(UnboundedCounter.get status.wind_speed)
      ~density:(BoundedCounter.get status.density)
      status.matrix ;
    let status =
      { status with
        ts = status.ts + 1 ;
      }
    in
    if status.ts > end_ts then None
    else match Event.pop status.events with
      | None -> Some status
      | Some (event,events) ->
        let status = { status with events = events } in
        match Event.interpret event with
        | `PageUp -> Some { status with
                            wind_speed = UnboundedCounter.decr status.wind_speed }
        | `PageDown -> Some { status with
                              wind_speed = UnboundedCounter.incr status.wind_speed }
        | `Up -> Some { status with
                        frame_rate = UnboundedCounter.incr status.frame_rate }
        | `Down -> Some { status with
                          frame_rate = UnboundedCounter.decr status.frame_rate }
        | `Left -> Some { status with
                          density = BoundedCounter.decr status.density }
        | `Right -> Some { status with
                           density = BoundedCounter.incr status.density }
        | `Quit -> None
        | `Resize (w_term,h_term) ->
          let w0 = status.w and h0 = status.h in
          let w1 = w_term/2 and h1 = h_term in
          let matrix = Matrix.adapt ~w0 ~h0 ~w1 ~h1 status.matrix in
          Some { status with matrix ; w = w1 ; h = h1 }
        | `Ignore -> Some status

  let close {term;_} =
    Notty_lwt.Term.release term ;%lwt
    Lwt.return ()

end

let run =
  let rec aux status =
    Status.display status ;%lwt
    match Status.update status with
    | Some status -> aux status
    | None -> Lwt.return status
  in
  aux

let () =
  Status.init () >>= run >>= Status.close |> Lwt_main.run
