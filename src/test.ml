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

let cells = Array.init h (fun _ -> Array.init w (fun _ -> None))
let offset = ref 0

let step =
  fun () ->
    let fresh_offset = if !offset>0 then !offset-1 else !offset-1+h in
    let line = Array.get cells !offset
    and fresh_line = Array.get cells fresh_offset in
    let update key value =
      let value' = match value with
        | Some (_,colour) ->
          let colour = colour *. (0.82 +. Random.float 0.18) in
          if colour>0.07 then Some (pick (),colour) else None
        | None ->
          if Random.float 1. < density
          then Some (pick (),1.)
          else None
      in
      Array.set fresh_line key value'
    in
    Array.iteri update line ;
    offset := fresh_offset ;
    Lwt.return ()

let index colour =
  let r,g,b =
    if colour<0.10 then 0,0,0
    else if colour<0.19 then 0,1,0
    else if colour<0.28 then 0,2,0
    else if colour<0.37 then 0,3,0
    else if colour<0.46 then 0,4,0
    else if colour<0.55 then 0,5,0
    else if colour<0.64 then 1,5,1
    else if colour<0.73 then 2,5,2
    else if colour<0.82 then 3,5,3
    else if colour<0.91 then 4,5,4
    else 5,5,5
  in
  LTerm_style.index ((r * 6 + g) * 6 + b + 16)

let print =
  let print_cell cell () = match cell with
    | None ->
      LTerm.prints LTerm_text.(eval [S "　"])
    | Some (code,colour) ->
      let utf8 = utf8_of_code code in
      LTerm.prints LTerm_text.(eval [
          B_fg (index colour) ;
          S (Format.sprintf "%s" utf8) ;
          E_fg ;
        ])
  in
  let print_line line () =
    Array.fold_left (fun x cell -> x >>= print_cell cell)
      (Lwt.return ()) line ;%lwt
    LTerm.printlf ""
  in
  let rec aux n () =
    if n<h then begin
      let n' = if n + !offset < h then n + !offset else n + !offset - h in
      print_line (Array.get cells n') () >>= aux (n+1)
    end else Lwt.return ()
  in
  (*
  fun () -> LTerm.printlf "[%d]@." !offset >>= aux 0
   *)
  aux 0

let () =
  let rec aux () =
    Lwt_unix.sleep frame_rate >>= step >>= print >>= aux
  in
  aux () |> Lwt_main.run
