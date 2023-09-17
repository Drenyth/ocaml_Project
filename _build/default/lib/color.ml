(** Color abstraction for our window manager.
    Can provide back and forth operations with RGB and internal representations
*)

open Base

type t = Int.t [@@deriving show]

let from_rgb r g b =
  let bg = g lsl 8 in
  let br = r lsl 16 in
  (br lor (bg lor b))

let to_rgb t =
  let b = t land 255 in
  let g = t land 65280 in
  let r = t land 16711680 in
  (r lsr 16,g lsr 8, b)

let to_int t  = Int.to_int t

let inverse t = let (r,g,b) = to_rgb t in
  let rr = 255 - r in
  let gg = 255 - g in
  let bb = 255 - b in
  from_rgb rr gg bb

let random () =
  let r = Random.int 255 in
  let g = Random.int 255 in
  let b = Random.int 255 in
  from_rgb r g b


(** add 2 color component-wise: *)
(** the result is a valid color  *)
let (+) c1 c2 =
  let (r,g,b) = to_rgb c1 in
  let (r1,g1,b1) = to_rgb c2 in
  let r = Int.min (r+r1) 255 in
  let g = Int.min (g+g1) 255 in
  let b = Int.min (b+b1) 255 in
  (from_rgb r g b)
  

let white = from_rgb 255 255 255
let black = from_rgb 0 0 0
let red   = from_rgb 255 0 0
let green = from_rgb 0 255 0
let blue  = from_rgb 0 0 255

let select_couleur () = Stdio.printf "\nVeuillez entrer une des couleurs suivantes : Bleu, Vert, Rouge ou Blanc\n%!"; let couleur = Stdio.In_channel.input_line Stdio.stdin in begin match couleur with
    |Some line -> begin match line with
      |"Bleu" -> blue
      |"Vert" -> green
      |"Rouge" -> red
      |"Blanc" ->  white
      |_ -> random () end
    |None -> Stdio.printf"Probleme avec la lecture de l'user input\n"; random () end

let%test "idint" =
  let c = random () in
  to_int c = c

let%test "idrgb" =
  let c = random () in
  let (r,g,b) = to_rgb c in
  from_rgb  r g b = c

let%test "white" =
  let (r,g,b) = to_rgb white in
  (r = 255) && (g=255) && (b=255)

let%test "black/white" = white = inverse black

let%test "whitecolors" = (red + green + blue) = white

let%test "addwhite" = white = white + white
