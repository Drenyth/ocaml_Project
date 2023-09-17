type direction = Vertical | Horizontal [@@deriving show]
type window = Win of string * Color.t [@@deriving show]
                                

type coordinate = Coord of {px: int; py: int; sx: int; sy: int} [@@deriving show]
type split = Split of direction * float (* ratio between 0 and 1 *) [@@deriving show]


let draw_win w coord bc = begin match coord with
  |Coord{px;py;sx;sy} -> match  w with
    |Win(_,y) -> match bc with
      |c -> Graphics.set_color (Color.to_int y); Graphics.fill_rect px py sx sy; Graphics.set_color (Color.to_int c); Graphics.set_line_width 5; Graphics.draw_rect px py sx sy end
      
type wmtree = ((split * coordinate), (window * coordinate)) Tree.t [@@deriving show]
type wmzipper = ((split * coordinate), (window * coordinate)) Tree.z [@@deriving show]

let get_coord wt = begin match wt with
  |Tree.Leaf (_,y) -> y
  |Tree.Node((_,c),_,_) -> c end

let change_coord coord wt = begin match wt with
  |Tree.Leaf(x,_) -> Tree.Leaf(x,coord)
  |Tree.Node((a,_),b,d) -> Tree.Node((a,coord),b,d) end

let rec draw_wmtree bc wt = begin match wt with
  |Tree.Leaf(x,y) -> draw_win x y bc
  |Tree.Node((_,_),c,d) -> draw_wmtree bc c; draw_wmtree bc d end

let draw_wmzipper bc wz = begin match wz with
  |Tree.TZ(_,y) -> draw_wmtree bc y end

(**returns the 4 int in a coord*)
let recup_coord coord = match coord with
  |Coord{px;py;sx;sy} -> (px,py,sx,sy)

(**multiplies a float with an int and return an int *)
let multiply_ratio_coord ratio c = Float.to_int(Float.mul ratio (Int.to_float c))

(** multiplies the coordinates of the right side of the desktop *)
(** when the split is Horizontal *)
let multiply_ratio_horizontal_r ratio c = let (px,py,sx,sy) = recup_coord c in
  Coord{px=px + (multiply_ratio_coord ratio sx);py=py;sx=(multiply_ratio_coord (1.0 -. ratio) sx);sy=sy}

let multiply_ratio_horizontal_l ratio c =
	let (px,py,sx,sy) = recup_coord c in
	Coord{px=px;py=py;sx=(multiply_ratio_coord ratio sx);sy=sy}

(** multiplies the coordinates of the right side of the desktop *)
(** when the split is Vertical *)
let multiply_ratio_vertical ratio c = let (px,py,sx,sy) = recup_coord c in
 Coord{px=px;py=py + (multiply_ratio_coord ratio sy);sx=sx;sy=(multiply_ratio_coord (1.0 -. ratio) sy)}

let multiply_ratio_vertical_l ratio c =
	let (px,py,sx,sy) = recup_coord c in
 Coord{px=px;py=py;sx=sx;sy=(multiply_ratio_coord ratio sy)}

let rec update_coord c t =
  match Base.phys_equal (Tree.is_leaf t) true  with
  |true -> change_coord c t                                       (*t est une feuille donc on change ses coordonnées directement*)
  |false -> match Tree.get_node_data t with                       (*t est une node donc on doit identifier les différents cas*)
    |None -> Stdio.printf "\nErreur update_coord, match get_node_data t\n%!";
      t
    |Some(Split(dirp,ratiop),cp) -> match dirp with
      |Horizontal -> let ncp = multiply_ratio_horizontal_r ratiop cp in begin match t with (*CAS SPLIT HORIZONTAL*)
        |Leaf _ -> Stdio.printf "\nErreur update_coord, match t 1\n%!";
          t
        |Node(v,l1,l2) -> begin match (Base.phys_equal (Tree.is_leaf l1) true) && (Base.phys_equal (Tree.is_leaf l2) true) with
            |true -> Tree.combine v (change_coord c l1) (change_coord ncp l2)
            |false -> begin match Base.phys_equal (Tree.is_leaf l1) true with 	(* Le fils gauche est une feuille et le fils droit est un noeud *)
                |true -> begin match Tree.get_node_data l2 with
                    |None -> Stdio.printf "\nErreur update_coord, match get_node_data l2 1\n%!";
                      t
                    |Some(Split(dirl,ratiol),_) -> begin match dirl with
                        | Horizontal ->
                          Tree.combine v (change_coord c l1) (update_coord (multiply_ratio_horizontal_l ratiol ncp) (change_coord ncp l2))
                        | Vertical ->
                          Tree.combine v (change_coord c l1) (update_coord (multiply_ratio_vertical_l ratiol ncp) (change_coord ncp l2))
                      end
                  end
                |false -> begin match Base.phys_equal (Tree.is_leaf l2) true with	(* Le fils gauche est un noeud et le fils droit est une feuille n*)
                    |true -> begin match Tree.get_node_data l1 with
                        |None -> Stdio.printf "\nErreur update_coord, match get_node_data l1 1\n%!";
                          t
                        |Some(Split(dirl,ratiol),_) -> begin match dirl with
                            |Horizontal -> Tree.combine v (update_coord (multiply_ratio_horizontal_l ratiol c) (change_coord c l1)) (change_coord ncp l2)
                            |Vertical -> Tree.combine v (update_coord (multiply_ratio_vertical_l ratiol c) (change_coord c l1)) (change_coord ncp l2)
                          end
                      end
                    |false -> begin match Tree.get_node_data l1 with (* Les 2 fils sont des noeuds *)
                        | None -> Stdio.printf "\nErreur update_coord, match get_node data l1 2\n%!";
                          t
                        |Some(Split(dirlg,ratiolg),_) -> begin match dirlg with
                            |Horizontal -> begin match Tree.get_node_data l2 with
                                |None -> Stdio.printf "\nErreur update_coord, match get_node_data l2 2\n%!";
                                  t
                                |Some(Split(dirld,ratiold),_) -> begin match dirld with
                                    |Horizontal ->
                                      Tree.combine v (update_coord (multiply_ratio_horizontal_l ratiolg c) (change_coord c l1)) (update_coord (multiply_ratio_horizontal_l ratiold ncp) (change_coord ncp l2))
                                    |Vertical ->
                                      Tree.combine v (update_coord (multiply_ratio_horizontal_l ratiolg c) (change_coord c l1)) (update_coord (multiply_ratio_vertical_l ratiold ncp) (change_coord ncp l2))
                                  end
                              end
                            |Vertical -> begin match Tree.get_node_data l2 with
                                |None -> Stdio.printf "\nErreur update_coord, match get_node_data l2 3\n%!";
                                  t
                                |Some(Split(dirld,ratiold),_) -> begin match dirld with
                                    |Horizontal ->
                                      Tree.combine v (update_coord (multiply_ratio_vertical_l ratiolg c) (change_coord c l1)) (update_coord (multiply_ratio_horizontal_l ratiold ncp) (change_coord ncp l2))
                                    |Vertical ->
                                      Tree.combine v (update_coord (multiply_ratio_vertical_l ratiolg c) (change_coord c l1)) (update_coord (multiply_ratio_vertical_l ratiold ncp) (change_coord ncp l2))
                                  end
                              end
                          end
                      end
                  end
              end
          end
        end
      |Vertical -> let ncp = multiply_ratio_vertical ratiop cp in match t with        (*CAS SPLIT VERTICAL*)
				 |Leaf _ -> Stdio.printf "\nErreur update_coord, match t 2\n%!";
					     t
				 |Node(v,l1,l2) -> match (Base.phys_equal (Tree.is_leaf l1) true) && (Base.phys_equal (Tree.is_leaf l2) true) with
          |true -> Tree.combine v (change_coord c l1) (change_coord ncp l2)
          |false -> match Base.phys_equal (Tree.is_leaf l1) true with	(* Le fils gauche est une feuille et le fils droit est un noeud *)
            |true -> begin match Tree.get_node_data l2 with
						 |None -> Stdio.printf "\nErreur update_coord, match get_node_data l2 4\n%!";
							   t
						 |Some(Split(dirl,ratiol),_) -> begin match dirl with
							 | Horizontal -> Tree.combine v (change_coord c l1) (update_coord (multiply_ratio_horizontal_l ratiol ncp) (change_coord ncp l2))
							 | Vertical -> Tree.combine v (change_coord c l1) (update_coord (multiply_ratio_vertical_l ratiol ncp) (change_coord ncp l2))
							end
            end
            |false -> match Base.phys_equal (Tree.is_leaf l2) true with	(* Le fils gauche est un noeud et le fils droit est une feuille*)
              |true -> begin match Tree.get_node_data l1 with
                |None -> Stdio.printf "\nErreur update_coord, match get_node_data l1 3\n%!";
							   t
                |Some(Split(dirl,ratiol),_) -> begin match dirl with
                  |Horizontal -> Tree.combine v (update_coord (multiply_ratio_horizontal_l ratiol ncp) (change_coord ncp l1)) (change_coord c l2)
                  |Vertical -> Tree.combine v (update_coord (multiply_ratio_vertical_l ratiol ncp) (change_coord ncp l1)) (change_coord c l2)
							end
						end
            |false -> begin match Tree.get_node_data l1 with (* Les 2 fils sont des noeuds *)
						 |None -> Stdio.printf "\nErreur update_coord, match get_node_data l1 4\n%!";
							   t
						 |Some(Split(dirlg,ratiolg),_) -> begin match dirlg with
							 |Horizontal -> begin match Tree.get_node_data l2 with
								 |None -> Stdio.printf "\nErreur update_coord, match get_node_data l2 5\n%!";
									   t
								 |Some(Split(dirld,ratiold),_) -> begin match dirld with
									 |Horizontal -> Tree.combine v (update_coord (multiply_ratio_horizontal_l ratiolg c) (change_coord c l1)) (update_coord (multiply_ratio_horizontal_l ratiold ncp) (change_coord ncp l2))
									 |Vertical -> Tree.combine v (update_coord (multiply_ratio_horizontal_l ratiolg c) (change_coord c l1)) (update_coord (multiply_ratio_vertical_l ratiold ncp) (change_coord ncp l2))
									end
								end
							 |Vertical -> begin match Tree.get_node_data l2 with
								 |None -> Stdio.printf "\nErreur update_coord, match get_node_data l2 6\n%!";
									   t
								 |Some(Split(dirld,ratiold),_) ->
									begin match dirld with
									 |Horizontal -> Tree.combine v (update_coord (multiply_ratio_vertical_l ratiolg c) (change_coord c l1)) (update_coord (multiply_ratio_horizontal_l ratiold ncp) (change_coord ncp l2))
									 |Vertical -> Tree.combine v (update_coord (multiply_ratio_vertical_l ratiolg c) (change_coord c l1)) (update_coord (multiply_ratio_vertical_l ratiold ncp) (change_coord ncp l2))
									end
								end
							end
						end

let create_window_horizontal count oz default_ratio =
  match oz with
    |Some(Tree.TZ(context,tree)) -> begin match tree with
      |Tree.Leaf(Win(n,c),coordonnee) -> let (px,py,sx,sy) = recup_coord coordonnee in
        let nt = (Tree.combine (Split(Horizontal,default_ratio),coordonnee) (Tree.return(Win(n,c),(Coord{px=px;py=py;sx=Float.to_int(Float.mul default_ratio (Int.to_float sx));sy=sy}))) (Tree.return(Win("W" ^ (Int.to_string (count)), Color.random ()), (Coord{px=(px + Float.to_int(Float.mul default_ratio (Int.to_float sx)));py=py;sx=Float.to_int(Float.mul default_ratio (Int.to_float sx));sy=sy})))) in
          Some (Tree.TZ(context,nt));
      |Tree.Node _ -> oz
      end
    |None -> None

let create_window_vertical count oz default_ratio =
  match oz with
    |Some(Tree.TZ(context,tree)) -> begin match tree with
      |Tree.Leaf(Win(n,c),coordonnee) -> let (px,py,sx,sy) = recup_coord coordonnee in
        let nt = (Tree.combine (Split(Vertical,default_ratio),coordonnee) (Tree.return(Win(n,c),(Coord{px=px;py=py;sx=sx;sy=Float.to_int(Float.mul default_ratio (Int.to_float sy))}))) (Tree.return(Win("W" ^ (Int.to_string (count)), Color.random ()), (Coord{px=px;py=(py + Float.to_int(Float.mul default_ratio (Int.to_float sy)));sx=sx;sy=Float.to_int(Float.mul default_ratio (Int.to_float sy))})))) in
          Some (Tree.TZ(context,nt));
      |Tree.Node _ -> oz
    end
    |None -> None
