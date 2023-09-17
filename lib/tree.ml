open Base

type ('v, 'w) t =
  | Node of 'v * ('v,'w) t * ('v,'w) t
  | Leaf of 'w [@@deriving show]

let return w = Leaf w

let combine v l1 l2 = Node(v,l1,l2)
 
let is_leaf = fun w -> match w with
  |Node(_,_,_) -> false
  |Leaf _ -> true

let get_leaf_data = fun w -> let value = is_leaf w in
  if phys_equal value  true then match w with
    |Node(_,_,_) -> None
    |Leaf x -> Some x
  else None

let get_node_data = fun w -> let value = is_leaf w in
  if phys_equal value false then match w with
    |Node(x,_,_) -> Some x
    |Leaf _ -> None
  else None

let rec map (f,g) d = match d with
  |Node(x,y,z) -> Node((f x), (map (f,g) y), (map (f,g) z))
  |Leaf x -> Leaf (g x)

let rec iter (f,g) d = match d with
  |Node(x,y,z) -> let () = f x in (); iter(f,g) y; iter(f,g) z
  |Leaf x ->  let () = g x in ()

type ('v, 'w) z = TZ of ('v,'w) context * ('v,'w) t
and ('v,'w) context =
  | Top
  | LNContext of 'v * ('v,'w) context * ('v,'w) t
  | RNContext of ('v,'w) t * 'v * ('v,'w) context [@@deriving show]


let from_tree d = match d with
  |Node(x,y,z) -> TZ(Top,Node(x,y,z))
  |Leaf x -> TZ(Top,Leaf x)

let change z s = match z with
  |TZ(x,_) -> TZ(x,s)
             

let change_up z v = match z with
  |TZ(LNContext(_,b,c),x) -> TZ(LNContext(v,b,c),x)
  |TZ(RNContext(a,_,c),x) -> TZ(RNContext(a,v,c),x)
  |TZ(Top,x) -> TZ(Top,x)


let go_down z = match z with
  | TZ(_, Leaf _) -> None
  | TZ(p, Node(a,b,c)) -> Some (TZ(LNContext(a, p,c),b))
                       
let go_up z = match z with
  | TZ(p, a) -> begin match p with
    | Top -> None
    | LNContext(m,n,o) -> Some (TZ(n, (combine m a o)))
    | RNContext(m,n,o) -> Some (TZ(o,(combine n m a))) end

let go_left z = match z with
  |TZ(Top,_) -> None
  |TZ(LNContext(_,_,_),_) -> None
  |TZ(RNContext(x,y,z),a) -> Some (TZ(LNContext(y,z,a),x))

let go_right z = match z with
  |TZ(Top,_) -> None
  |TZ(LNContext(x,y,z),a) -> Some (TZ(RNContext(a,x,y),z))
  |TZ(RNContext(_,_,_),_) -> None

let  rec reflexive_transitive f d = match d with
  |TZ(_,_) -> match f d with
    |Some x -> reflexive_transitive f x
    |None -> d

let focus_first_leaf t = let tz = from_tree t in
  reflexive_transitive go_down tz

let remove_leaf z = match z with
  |TZ(a,Leaf _) -> begin match a with
      |LNContext(x,y,z) -> Some (TZ(y,z),x)
      |RNContext(x,y,z) -> Some (TZ(z,x),y)
      |Top -> None end
  |TZ(_,Node(_,_,_)) -> None

let is_left_context  z = match z with
  |TZ(LNContext(_,_,_),_) -> true
  |TZ(RNContext(_,_,_),_) -> false
  |TZ(Top,_) -> false
     
let is_right_context z = match z with
  |TZ(LNContext(_,_,_),_) -> false
  |TZ(RNContext(_,_,_),_) -> true
  |TZ(Top,_) -> false
    
let is_top_context   z = match z with
  |TZ(LNContext(_,_,_),_) -> false
  |TZ(RNContext(_,_,_),_) -> false
  |TZ(Top,_) -> true

let rec move_until f p z = match z with
  |TZ(_,_) -> begin match (f z) with
  |Some x -> begin match (p z) with
      |true -> Some z
      |false -> move_until f p x end
  |None -> Some z end

let rec go_downr z =
	match (go_down z) with
		|Some a -> begin match (go_right a) with
			|Some b -> begin match b with
				| TZ(_,Leaf _) -> Some b
				| TZ(_,Node(_,_,_)) -> go_downr b
     end
      |None -> None
    end
    |None -> None

let rec next_leaf z = 
  match go_right z with
    |Some t ->
      begin match t with
        | TZ(_,Leaf _) -> Some t
        | TZ(_,Node(_,_,_)) -> Some (reflexive_transitive go_down t)
      end
    |None -> begin match go_up z with
        |Some a -> begin match is_top_context a with
            |true -> begin match (go_downr z) with
                |Some a -> Some(reflexive_transitive go_down a)
                |None -> None
              end 
            |false -> next_leaf a
          end
        |None -> begin match (go_downr z) with
            |Some a -> Some (reflexive_transitive go_down a)
            |None -> None
          end 
      end

let rec previous_leaf z = 
	match go_left z with
	  |Some t -> begin match t with
		  |TZ(_,Leaf _) -> Some t
		  |TZ(_,Node(_,_,_)) -> go_downr t
		  end
    |None -> begin match (go_up z) with
      |Some a -> previous_leaf a
      |None -> None
      end 
                    
let%test "n" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine 4 l1 l2 in
  let n2 = combine 5 n1 l3 in
  Stdlib.(n2 = (Node(5,Node(4, Leaf 1, Leaf 2), Leaf 3)))

let%test "leaf1" = is_leaf (Leaf 1)
let%test "leaf2" = is_leaf (Node (1, Leaf 1, Leaf 1)) |> not

let%test "gld1" =  match get_leaf_data (Leaf 1) with
  | None -> false
  | Some o -> Int.(o = 1)

let%test "gld2" = match get_leaf_data (Node (1, Leaf 2, Leaf 3)) with
  | None -> true
  | _ -> false

let%test "gnd1" =  match get_node_data (Leaf 1) with
  | None -> true
  | _ -> false

let%test "gnd2" = match get_node_data (Node (1, Leaf 2, Leaf 3)) with
  | None -> false
  | Some o -> Int.(o = 1)

let%test "map" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine "four" l1 l2 in
  let n2 = combine "five" n1 l3 in
  let g x = x * 2 in
  let f x = x ^ x in
  let n3 = map (f,g) n2 in
  Stdlib.(n3 = (Node("fivefive",Node("fourfour", Leaf 2, Leaf 4), Leaf 6)))

let%test "gd1" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine "four" l1 l2 in
  let n2 = combine "five" n1 l3 in
  let z = from_tree n2 in
  match go_down z with
  | Some z' -> Stdlib.(z' = TZ (LNContext ("five", Top,Leaf 3),Node("four", Leaf 1, Leaf 2)))
  | None -> false

let%test "gd2" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine "four" l1 l2 in
  let n2 = combine "five" n1 l3 in
  let z = from_tree n2 in
  match Option.(Some z >>= go_down >>= go_down) with
  | Some z' -> Stdlib.(z' = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1))
  | None -> false


let%test "gd3" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine "four" l1 l2 in
  let n2 = combine "five" n1 l3 in
  let z = from_tree n2 in
  match Option.(Some z >>= go_down >>= go_down >>= go_down) with
  | Some _ -> false
  | None -> true


let%test "gu1" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match go_up z with
  | Some z' -> Stdlib.(z' = TZ (LNContext ("five", Top,Leaf 3),Node("four", Leaf 1, Leaf 2)))
  | None -> false

let%test "gu2" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match Option.(Some z >>= go_up >>= go_up) with
  | Some z' -> Stdlib.(z' = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)))
  | None -> false

let%test "gu3" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match Option.(Some z >>= go_up >>= go_up >>= go_up) with
  | Some _  -> false
  | None -> true

let%test "gl1" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match go_left z with
  | Some z' -> Stdlib.(z' = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1))
  | None -> false

let%test "gl2" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match go_left z with
  | Some _ -> false
  | None -> true

let%test "gr1" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match go_right z with
  | Some z' -> Stdlib.(z' = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2))
  | None -> false

let%test "gr2" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match go_right z with
  | Some _ -> false
  | None -> true

let%test "rf1" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  Stdlib.(reflexive_transitive go_up z = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)))

let%test "rf2" =
  let z =   TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  Stdlib.(reflexive_transitive go_up z = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)))


let%test "rf3" =
  let z = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)) in
  Stdlib.(reflexive_transitive go_up z = z)


let%test "ffl1" =
  let t = Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3) in
  Stdlib.(focus_first_leaf t = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1))


let%test "rl1" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match remove_leaf z with
  | None -> false
  | Some (z, v) -> Stdlib.((z = TZ (LNContext ("five", Top, Leaf 3), Leaf 2)) && (v="four"))


let%test "rl2" =
  let z = TZ (LNContext ("five", Top,Leaf 3),Node("four", Leaf 1, Leaf 2)) in
  match remove_leaf z with
  | None -> true
  | _ -> false

let%test "mv1" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  let p = fun (TZ(_,s)) -> match get_node_data s with | None -> false | Some v -> String.(v = "five") in
  match move_until go_up p z with
  | None -> false
  | Some z -> Stdlib.(z = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)))

let%test "mv2" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  let p = fun (TZ(_,s)) -> match get_node_data s with | None -> false | Some v -> String.(v = "four") in
  match move_until go_up p z with
  | None -> false
  | Some z -> Stdlib.(z = TZ (LNContext ("five", Top,Leaf 3),Node("four", Leaf 1, Leaf 2)))

let%test "nl1" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match next_leaf z with
  | None -> false
  | Some z -> Stdlib.(z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2))

let%test "nl2" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match Option.(Some z >>= next_leaf >>= next_leaf) with
  | None -> false
  | Some z -> Stdlib.(z = TZ (RNContext (Node("four", Leaf 1, Leaf 2), "five", Top), Leaf 3))


let%test "nl3" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match Option.(Some z >>= next_leaf >>= next_leaf >>= next_leaf) with
  | None -> true
  | _ -> false

let%test "pl1" =
  let z = TZ (RNContext (Node ("four", Leaf 1, Leaf 2), "five", Top), Leaf 3) in
  match previous_leaf z with
  | None -> false
  | Some z -> Stdlib.(z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2))

let%test "pl2" =
  let z = TZ (RNContext (Node ("four", Leaf 1, Leaf 2), "five", Top), Leaf 3) in
  match Option.(Some z >>= previous_leaf >>= previous_leaf) with
  | None -> false
  | Some z -> Stdlib.(z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1))


let%test "pl3" =
  let z = TZ (RNContext (Node ("four", Leaf 1, Leaf 2), "five", Top), Leaf 3) in
  match Option.(Some z >>= previous_leaf >>= previous_leaf >>= previous_leaf) with
  | None -> true
  | _ -> false
