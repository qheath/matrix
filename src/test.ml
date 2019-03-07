open Lwt

let () = Random.self_init ()

module Code : sig

  type t

  val pick : unit -> t
  val opt_to_uchar : t option -> Uchar.t

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

  let opt_to_uchar code_opt =
    let code = match code_opt with
      | Some code -> code
      | None -> default
    in
    Uchar.of_int code

end

module Cell : sig

  type t

  val make : unit -> t
  val update : density:float -> t -> t
  val to_image : t -> Notty.I.t

end = struct

  type t = (Code.t * float) option

  let make () = None

  let update ~density cell =
    if Random.float 1. < density
    then Some (Code.pick (),1.)
    else match cell with
      | Some (_,colour) ->
        let colour = colour *. (0.82 +. Random.float 0.18) in
        if colour>0.07 then Some (Code.pick (),colour) else None
      | None -> None

  let index colour =
    let point0,colour0 = Gg.V3.v 0. 0. 0., 0.
    and pcs = [
      Gg.V3.v 0. 1. 0., 0.3 ;
      Gg.V3.v 1. 1. 1., 1. ;
    ] in
    let r,g,b =
      let point =
        let rec aux (point0,colour0) = function
          | [] -> point0
          | (point1,colour1 as pc1)::l ->
            if colour<colour1 then Gg.V3.mix point0 point1 ((colour-.colour0)/.(colour1-.colour0))
            else aux pc1 l
        in
        if colour<colour0 then point0
        else aux (point0,colour0) pcs
      in
      Gg.V3.to_tuple point
    and cap x =
      let y = int_of_float @@ x *. 256. in
      if y<0 then 0
      else if y>255 then 255
      else y
    in
    Notty.A.rgb_888 ~r:(cap r) ~g:(cap g) ~b:(cap b)

  let to_image cell =
    let attr,code = match cell with
      | None ->
        Notty.A.empty,None
      | Some (code,colour) ->
        (Notty.A.fg @@ index colour),Some code
    in
    Notty.I.uchar attr (Code.opt_to_uchar code) 1 1

end

module Line : sig

  type t

  val make : w:int -> t
  val update_copy : density:float -> t -> t -> unit
  val to_image : w:int -> t -> Notty.I.t

end = struct

  type t = Cell.t array

  let make ~w =
    Array.init w (fun _ -> Cell.make ())

  let update_copy ~density old_line new_line =
    let update_copy_cell key value =
      Array.set new_line key @@ Cell.update ~density value
    in
    Array.iteri update_copy_cell old_line

  let to_image ~w line =
    Notty.I.tabulate w 1 (fun i _ -> Cell.to_image line.(i))

end

module Matrix : sig

  type t

  val make : h:int -> w:int -> t
  val step : h:int -> density:float -> t -> t
  val to_image : w:int -> h:int -> t -> Notty.I.t

end = struct

  type t = {
    lines: Line.t array ;
    old_offset : int ;
    new_offset : int ;
  }

  let make ~h ~w = {
    lines = Array.init h (fun _ -> Line.make ~w) ;
    old_offset = 0 ;
    new_offset = h-1 ;
  }

  let step ~h ~density {lines;old_offset;new_offset} =
    Line.update_copy
      ~density
      (Array.get lines old_offset)
      (Array.get lines new_offset) ;
    {
      lines ;
      old_offset = new_offset ;
      new_offset =
        let new_new_offset = new_offset-1 in
        if new_new_offset<0 then new_new_offset+h else new_new_offset ;
    }

  let to_image ~w ~h {lines;old_offset;_} =
    let aux _ i =
      let i' = i + old_offset in
      Line.to_image ~w lines.(if i'<h then i' else i'-h)
    in
    Notty.I.tabulate 1 h aux |> Notty_lwt.eol
    |> Notty_lwt.eol

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

  type t = [
    | `Key of Notty.Unescape.key
    | `Mouse of Notty.Unescape.mouse
    | `Paste of Notty.Unescape.paste
    | `Resize of int * int
  ]
  type stream

  val stream_of_term : Notty_lwt.Term.t -> stream
  val pop : stream -> (t * stream) option

end = struct

  type t = [
    | `Key of Notty.Unescape.key
    | `Mouse of Notty.Unescape.mouse
    | `Paste of Notty.Unescape.paste
    | `Resize of int * int
  ]

  type stream = t list * t Lwt_stream.t

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
    frame_rate : UnboundedCounter.t ;
    density : BoundedCounter.t ;
    ts : int ;
  }

  let init () =
    let term = Notty_lwt.Term.create () in
    let events = Event.stream_of_term term in
    let w,h = 90,50 in
    let matrix = Matrix.make ~w ~h in
    let frame_rate = UnboundedCounter.make ~lambda:2. 0.1
    and density = BoundedCounter.make ~lambda:0.1 0.01
    and ts = 0 in
    Lwt.return {
      term ;
      events ;
      w ;
      h ;
      matrix ;
      frame_rate ;
      density ;
      ts ;
    }

  let display {term;w;h;matrix;frame_rate;_} =
    Matrix.to_image ~w ~h matrix |> Notty_lwt.Term.image term ;%lwt
    Lwt_unix.sleep @@ UnboundedCounter.get frame_rate

  let end_ts = max_int

  let update status =
    let status =
      { status with
        matrix =
          Matrix.step ~h:status.h ~density:(BoundedCounter.get status.density) status.matrix ;
        ts = status.ts + 1 ;
      }
    in
    if status.ts > end_ts then None
    else match Event.pop status.events with
      | None -> Some status
      | Some (event,events) ->
        let status = { status with events = events } in
        match event with
        | `Key key ->
          begin match key with
            | `Arrow arrow,_ ->
              begin match arrow with
                | `Up -> Some { status with
                                frame_rate = UnboundedCounter.incr status.frame_rate }
                | `Down -> Some { status with
                                  frame_rate = UnboundedCounter.decr status.frame_rate }
                | `Left -> Some { status with
                                  density = BoundedCounter.decr status.density }
                | `Right -> Some { status with
                                   density = BoundedCounter.incr status.density }
              end
            | _ -> None
          end
        | `Paste _ | `Mouse _ -> Some status
        | `Resize _ -> None

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
