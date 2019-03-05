open Lwt

(*
let h,w = 24,80
let h,w = 15,50
 *)
let h,w = 26,39
let frame_rate = 0.2
let density = 0.01

let utf8_of_code code =
  let uchar = CamomileLibrary.UChar.of_int code in
  CamomileLibrary.UTF8.init 1 (fun _ -> uchar)

let code_of_utf8 utf8 =
  let uchar = CamomileLibrary.UTF8.get utf8 0 in
  CamomileLibrary.UChar.int_of uchar

module M = Map.Make(struct type t = int let compare = compare end)

let pick =
  Random.self_init () ;
  let choices = [
    "０","９" ;
    "Ａ","Ｚ" ;
    "ａ","ｚ" ;
    "ぁ","ゖ" ;
    "ァ","ヺ" ;
  ] in
  let map,_ =
    let rec add map key value n =
      if n>0 then
        add (M.add key value map) (key+1) (value+1) (n-1)
      else map,key
    in
    let aux (map,sum) (low,high) =
      let base_value = code_of_utf8 low in
      let sub_sum = code_of_utf8 high - base_value + 1 in
      add map sum base_value sub_sum
    in
    List.fold_left aux (M.empty,0) choices
  in
  fun () ->
    let f = Random.float 1. in
    let i = int_of_float (f *. (float_of_int (M.cardinal map))) in
    match M.find_opt i map with
    | None -> code_of_utf8 " "
    | Some c -> c

module Cell : sig

  type t

  val make : unit -> t
  val update : t -> t
  val print : t -> unit -> unit Lwt.t

end = struct

  type t = (int * float) option

  let make () = None

  let update = function
    | Some (_,colour) ->
      let colour = colour *. (0.82 +. Random.float 0.18) in
      if colour>0.07 then Some (pick (),colour) else None
    | None ->
      if Random.float 1. < density
      then Some (pick (),1.)
      else None

  let index colour =
    let point0,colour0 = Gg.V3.v 0. 0. 0., 0.
    and pcs = [
      Gg.V3.v 0. 1. 0., 0.5 ;
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
    LTerm_style.rgb (cap r) (cap g) (cap b)

  let print cell () = match cell with
    | None ->
      LTerm.prints LTerm_text.(eval [S "　"])
    | Some (code,colour) ->
      let utf8 = utf8_of_code code in
      LTerm.prints LTerm_text.(eval [
          B_fg (index colour) ;
          S (Format.sprintf "%s" utf8) ;
          E_fg ;
        ])

end

module Line : sig

  type t

  val make : w:int -> t
  val update_copy : t -> t -> unit
  val print : t -> unit -> unit Lwt.t

end = struct

  type t = Cell.t array

  let make ~w =
    Array.init w (fun _ -> Cell.make ())

  let update_copy old_line new_line =
    let update_copy_cell key value =
      Array.set new_line key @@ Cell.update value
    in
    Array.iteri update_copy_cell old_line

  let print line () =
    Array.fold_left (fun x cell -> x >>= Cell.print cell)
      (Lwt.return ()) line ;%lwt
    LTerm.printlf ""

end

module Matrix : sig

  type t

  val make : h:int -> w:int -> t
  val step : t -> t
  val print : t -> unit -> unit Lwt.t

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

  let fold f {lines;old_offset;_} =
    let rec aux n accum =
      if n<h+old_offset then begin
        let n' = if n < h then n else n - h in
        f (Array.get lines n') accum >>= aux (n+1)
      end else Lwt.return accum
    in
    fun seed -> aux old_offset seed

  let print = fold Line.print

end

let () =
  let rec aux matrix =
    Lwt_unix.sleep frame_rate ;%lwt
    let matrix = Matrix.step matrix in
    Matrix.print matrix () ;%lwt
    aux matrix
  in
  Matrix.make ~w ~h |> aux |> Lwt_main.run
