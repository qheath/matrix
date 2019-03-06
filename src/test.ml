open Lwt

(*
let h,w = 24,80
let h,w = 15,50
 *)
let h,w = 26,39
let frame_rate = 0.2
let density = 0.01

let () = Random.self_init ()

let default_code,pick_code =
  let code_of_utf8 utf8 =
    let uchar =
      match Uutf.decode (Uutf.decoder (`String utf8)) with
      | `Uchar c -> c
      | _ -> assert false
    in
    Uchar.to_int uchar
  and default_utf8 = "　"
  and utf8_choices = [
    "０","９" ;
    "Ａ","Ｚ" ;
    "ａ","ｚ" ;
    "ぁ","ゖ" ;
    "ァ","ヺ" ;
  ] in
  let default_code = code_of_utf8 default_utf8 in
  let pick_code =
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
      List.fold_left aux (Codes.empty,0) utf8_choices
    in
    fun () ->
      match Codes.find_opt (Random.int (Codes.cardinal map)) map with
      | None -> default_code
      | Some c -> c
  in
  default_code,pick_code

module Cell : sig

  type t

  val make : unit -> t
  val update : t -> t
  val to_image : t -> Notty.I.t

end = struct

  type t = (int * float) option

  let make () = None

  let update cell =
    if Random.float 1. < density
    then Some (pick_code (),1.)
    else match cell with
      | Some (_,colour) ->
        let colour = colour *. (0.82 +. Random.float 0.18) in
        if colour>0.07 then Some (pick_code (),colour) else None
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
        Notty.A.empty,default_code
      | Some (code,colour) ->
        (Notty.A.fg @@ index colour),code
    in
    Notty.I.uchar attr (Uchar.of_int code) 1 1

end

module Line : sig

  type t

  val make : w:int -> t
  val update_copy : t -> t -> unit
  val to_image : t -> Notty.I.t

end = struct

  type t = Cell.t array

  let make ~w =
    Array.init w (fun _ -> Cell.make ())

  let update_copy old_line new_line =
    let update_copy_cell key value =
      Array.set new_line key @@ Cell.update value
    in
    Array.iteri update_copy_cell old_line

  let to_image line =
    Notty.I.tabulate w 1 (fun i _ -> Cell.to_image line.(i))

end

module Matrix : sig

  type t

  val make : h:int -> w:int -> t
  val step : t -> t
  val to_image : t -> Notty.I.t

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

  let decr offset =
    let new_offset = offset-1 in
    if new_offset<0 then new_offset+h else new_offset

  let step {lines;old_offset;new_offset} =
    Line.update_copy
      (Array.get lines old_offset)
      (Array.get lines new_offset) ;
    {
      lines ;
      old_offset = new_offset ;
      new_offset = decr new_offset ;
    }

  let to_image {lines;old_offset;_} =
    let aux _ i =
      let i' = i + old_offset in
      Line.to_image lines.(if i'<h then i' else i'-h)
    in
    Notty.I.tabulate 1 h aux |> Notty_lwt.eol
    |> Notty_lwt.eol

end

let end_time = Unix.time () +. 3.
let end_ts = 100

let init () =
  let matrix = Matrix.make ~w ~h in
  let term = Notty_lwt.Term.create () in
  Lwt.return (term,matrix)

let close term =
  Notty_lwt.Term.release term ;%lwt
  Lwt.return ()

let run (term,matrix) =
  let rec aux ts (matrix) =
    (*
  let events = Notty_lwt.Term.events term in
    begin match%lwt Lwt_stream.get events with
      | None -> Lwt.return ()
      | Some _ -> Lwt.return ()
    end ;%lwt
     *)
    Matrix.to_image matrix |> Notty_lwt.Term.image term ;%lwt
    Lwt_unix.sleep frame_rate ;%lwt
    (*
     *)
    if Unix.time () > end_time || ts > end_ts
    then Lwt.return term
    else (Matrix.step matrix) |> aux (ts+1)
  in
  aux 0 (matrix)

let () =
  init () >>= run >>= close |> Lwt_main.run
