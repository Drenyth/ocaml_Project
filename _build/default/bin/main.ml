open Base
open Stdio

open Ocamlwm23

let main () =
  let width = 640 in
  let height = 480 in
  let default_ratio = 0.5 in
  
  let gray = Color.from_rgb 128 128 128 in
  let cyan = Color.from_rgb 0 255 255 in
  let magenta = Color.from_rgb 255 0 255 in
  let teal = Color.from_rgb 0 128 128 in
  let purple = Color.from_rgb 128 0 128 in
  let olive = Color.from_rgb 128 128 0 in

  let active_color = Color.white in
  let inactive_color = Color.black in

  (* never increase ration above 0.95 or decrease below 0.05 *)
  let inc_ratio ratio = Float.min 0.95 (ratio +. 0.05) in
  let dec_ratio ratio = Float.max 0.05 (ratio -. 0.05) in

  (* create a new window *)
  let init_win count () =
    let w = Wm.Win("W" ^ (Int.to_string count), Color.random ()) in
    let c = Wm.Coord {px=0; py=0; sx=width;sy=height} in
    Tree.return  (w,c)  |> Tree.focus_first_leaf
  in

  (* create the canvas to draw windows *)
  let count = 0 in
  let f = Printf.sprintf " %dx%d" width height in
  let () = Graphics.open_graph f in

  (* event loop *)
  let rec loop oz count =
    (match oz with
     | None -> Stdio.printf "\nZERO WINDOW\n%!"
     | Some z -> Stdio.printf "\n%s\n%!" (Wm.show_wmzipper z)
    );

    match Graphics.read_key () with
    |'q' ->
      Stdio.printf "Total number of created windows: %d\nBye\n%!" count;
      raise Caml.Exit
    | 'h' ->
      Stdio.printf "\nhorizontal\n%!";
      begin
        if phys_equal count 0 then
          let newzipper = init_win count () in begin match newzipper with
            |Tree.TZ(_,Tree.Leaf(window,coordonnee)) -> Wm.draw_win window coordonnee active_color;
              loop (Some(newzipper)) (count + 1); (* we go start again the event loop *)
            |Tree.TZ(_) -> Stdio.printf "\nErreur begin match newzipper (h)\n%!";
              loop (Some(newzipper)) count; (* we go start again the event loop *)
          end
        else
          let newzipoption = Wm.create_window_horizontal count oz default_ratio in begin match newzipoption with
            |Some(zip) -> begin match Tree.go_down zip with
            |Some(TZ(_,Tree.Leaf(window,coordonnee))) -> Wm.draw_win window coordonnee inactive_color ; 
                begin match Tree.go_downr zip with
                  |Some(TZ(_,(Tree.Leaf(window,coordonnee)))) -> Wm.draw_win window coordonnee active_color;
                    let newzipoption = Tree.go_downr zip in
                     loop newzipoption (count+1); (* we go start again the event loop *)
                  |Some(TZ(_,Tree.Node _)) -> Stdio.printf "\nErreur begin match Tree.go_downr zip 1 (h)\n%!";
                     loop newzipoption count; (* we go start again the event loop *)
                  |None -> Stdio.printf"\nErreur begin match Tree.go_downr zip 2 (h)\n%!";
                      loop newzipoption count; (* we go start again the event loop *)
                end
            | Some(TZ _) -> Stdio.printf "\nErreur begin match go_down zip (h)\n%!";
              loop newzipoption count; (* we go start again the event loop *)
            | None -> Stdio.printf "\nErreur begin match go_down zip (h)\n%!";
              loop newzipoption count; (* we go start again the event loop *) 
          end
            |None -> Stdio.printf "\nErreur begin match go_down zip (h)\n%!";
              loop newzipoption count; (* we go start again the event loop *)
          end
      end
          
    | 'v' ->
      Stdio.printf "\nvertical\n%!";
      begin
         if phys_equal count 0 then
          let newzipper = init_win count () in begin match newzipper with
            |Tree.TZ(_,Tree.Leaf(window,coordonnee)) -> Wm.draw_win window coordonnee active_color;
              loop (Some(newzipper)) (count + 1); (* we go start again the event loop *)
            |Tree.TZ(_) -> Stdio.printf "\nErreur begin match newzipper (v)\n%!";
              loop (Some(newzipper)) count; (* we go start again the event loop *)
          end
        else
          let newzipoption = Wm.create_window_vertical count oz default_ratio in begin match newzipoption with
            |Some(zip) -> begin match Tree.go_down zip with
              |Some(TZ(_,Tree.Leaf(window,coordonnee))) -> Wm.draw_win window coordonnee inactive_color; 
                begin match Tree.go_downr zip with
                  |Some(TZ(_,(Tree.Leaf(window,coordonnee)))) -> Wm.draw_win window coordonnee active_color;
                    let newzipoption = Tree.go_downr zip in
                     loop newzipoption (count+1); (* we go start again the event loop *)
                  |Some(TZ(_,Tree.Node _)) -> Stdio.printf "\nErreur begin match Tree.go_downr zip 1 (v)\n%!";
                     loop newzipoption count;(* we go start again the event loop *)
                  |None -> Stdio.printf"\nErreur begin match Tree.go_downr zip 2 (v)\n%!";
                    loop newzipoption count;(* we go start again the event loop *)
                end
              |Some(TZ _) -> Stdio.printf "\nErreur begin match go_down zip (v)\n%!";
                loop newzipoption count;(* we go start again the event loop *)
              |None -> Stdio.printf "\nErreur begin match go_down zip (v)\n%!";
                loop newzipoption count;(* we go start again the event loop *)    
            end
          |None -> Stdio.printf "\nErreur begin match newzipoption (v)\n%!";
              loop newzipoption count;(* we go start again the event loop *)
          end
      end


    | 'n' ->
      Stdio.printf "\nnext\n%!";
      begin
        begin match oz with
          |Some (Tree.TZ(Top,Tree.Leaf _)) -> Stdio.printf "\nIl n'y a aucune fenêtres ou une seule. Veuillez créer de nouvelles fenêtres\n%!";
            loop oz count; (* we go start again the event loop *)
          |None -> Stdio.printf "\nIl n'y a aucune fenêtres ou une seule. Veuillez créer de nouvelles fenêtres\n%!";
            loop oz count; (* we go start again the event loop *)
          |Some (Tree.TZ(context,Tree.Node (desktop,treeg,treed))) -> Stdio.printf "\nErreur begin match oz 1 (n)\n%!";
            loop (Tree.go_down (Tree.TZ(context,(Tree.Node (desktop,treeg,treed))))) count;  (* correction mouvement sur une fenêtre + loop *)
          |Some (Tree.TZ(context,Tree.Leaf(window,coordonnee))) -> 
            if phys_equal (Tree.next_leaf (Tree.TZ(context,Tree.Leaf(window,coordonnee)))) None 
              then begin
                Stdio.printf "\nVous ne pouvez pas faire next_leaf, c'est la dernière feuille\n%!";
                loop oz count; (* we go start again the event loop *)
              end  
            else
              Wm.draw_win window coordonnee inactive_color;
              let oz = Tree.next_leaf (Tree.TZ(context,Tree.Leaf(window,coordonnee))) in begin match oz with
                |Some(Tree.TZ(_,t)) -> begin match Tree.get_leaf_data t with
                    |Some (window,_) -> Wm.draw_win window (Wm.get_coord t) active_color;
                      loop oz count; (* we go start again the event loop *)
                    |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data t (n)\n%!";
                      loop (Some(Tree.TZ(context,Tree.Leaf(window,coordonnee)))) count; (* we go start again the event loop *)
                    end
                |None -> Stdio.printf "\nErreur begin match 2 (n)\n%!";
                  loop (Some(Tree.TZ(context,Tree.Leaf(window,coordonnee)))) count; (* we go start again the event loop *) 
              end 
            end  
        end

    | 'p' ->
      Stdio.printf "\nprevious\n%!";
      begin
        begin match oz with
          |Some (Tree.TZ(Top,Tree.Leaf _)) -> Stdio.printf "\nIl n'y a aucune fenêtres ou une seule. Veuillez créer de nouvelles fenêtres\n%!";
            loop oz count; (* we go start again the event loop *)
          |None -> Stdio.printf "\nIl n'y a aucune fenêtres ou une seule. Veuillez créer de nouvelles fenêtres\n%!";
            loop oz count; (* we go start again the event loop *)
          |Some (Tree.TZ(context,Tree.Node (desktop,treeg,treed))) -> Stdio.printf "\nErreur begin match oz 1 (p)\n%!";
            loop (Tree.go_down (Tree.TZ(context,(Tree.Node (desktop,treeg,treed))))) count; (* correction mouvement sur une fenêtre + loop *)
          |Some (Tree.TZ(context,Tree.Leaf(window,coordonnee))) -> 
            if phys_equal (Tree.previous_leaf (Tree.TZ(context,Tree.Leaf(window,coordonnee)))) None 
              then begin
                Stdio.printf "\nVous ne pouvez pas faire previous_leaf, c'est la première feuille\n%!";
                loop oz count; (* we go start again the event loop *)
              end
            else
              Wm.draw_win window coordonnee inactive_color;
              let oz = Tree.previous_leaf (Tree.TZ(context,Tree.Leaf(window,coordonnee))) in begin match oz with
                |Some(Tree.TZ(_,t)) -> begin match Tree.get_leaf_data t with
                    |Some (window,_) -> Wm.draw_win window (Wm.get_coord t) active_color;
                      loop oz count; (* we go start again the event loop *)
                    |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data t (p)\n%!";
                      loop (Some(Tree.TZ(context,Tree.Leaf(window,coordonnee)))) count; (* we go start again the event loop *)
                    end
                |None -> Stdio.printf "\nErreur begin match oz 2 (p)\n%!";
                  loop (Some(Tree.TZ(context,Tree.Leaf(window,coordonnee)))) count; (* we go start again the event loop *)    
              end
            end  
        end


    | '+' ->
      Stdio.printf "\nincrement size\n%!";
      begin
        begin match oz with
          |None -> Stdio.printf "\nAucune fenêtre de créée\n%!";
            loop oz count; (* we go start again the event loop *)
          |Some(Tree.TZ(Top,_)) -> Stdio.printf "\nAction impossible, vous avez créé qu'une seule fenêtre\n%!";
            loop oz count; (* we go start again the event loop *)
          |Some(zip) -> begin match Tree.is_left_context zip with
            |true -> begin match Tree.go_up zip with
              |None -> Stdio.printf "\nErreur begin match go_up zip 1 (+)\n%!";
                  loop oz count; (* we go start again the event loop *)
              |Some(Tree.TZ(context,node)) -> begin match Tree.get_node_data node with
                |None -> Stdio.printf "\nErreur begin match Tree.get_node_data node 1 (+)\n%!";
                  loop oz count; (* we go start again the event loop *)
                |Some(Split(direction,ratio),c) -> let zipn = Tree.change_up zip (Split(direction,inc_ratio ratio),c) in
                  begin match Tree.go_up zipn with
                    |None -> Stdio.printf "\nErreur begin match go_up zipn 2 (+)\n%!";
                      loop oz count; (* we go start again the event loop *)
                    |Some(Tree.TZ(_,nodes)) -> begin match direction with
                      |Vertical -> let oz = Tree.TZ(context,(Wm.update_coord (Wm.multiply_ratio_vertical_l (inc_ratio ratio) c) nodes)) in
                        Wm.draw_wmzipper inactive_color oz;
                        let oz = Tree.go_down oz in
                        begin match oz with
                          |None -> Stdio.printf "\nErreur begin match go_down oz (+)\n%!";
                            loop oz count; (* we go start again the event loop *)
                          |Some(Tree.TZ(_,leaf)) -> begin match Tree.get_leaf_data leaf with
                            |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data leaf 1 (+)\n%!";
                              loop oz count; (* we go start again the event loop *)
                            |Some(window,coordonnee) -> Wm.draw_win window  coordonnee active_color;
                                    loop oz count; (* we go start again the event loop *)
                          end
                        end
                      |Horizontal -> let oz = Tree.TZ(context,(Wm.update_coord (Wm.multiply_ratio_horizontal_l (inc_ratio ratio) c) nodes)) in
                        Wm.draw_wmzipper inactive_color oz;
                        let oz = Tree.go_down oz in
                        begin match oz with
                          |None -> Stdio.printf "\nErreur begin match Tree.go_down oz (+)\n%!";
                            loop oz count; (* we go start again the event loop *)
                          |Some(Tree.TZ(_,leaf)) -> begin match Tree.get_leaf_data leaf with
                            |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data leaf 2 (+)\n%!";
                              loop oz count; (* we go start again the event loop *)
                            |Some(window,coordonnee) -> Wm.draw_win window  coordonnee active_color;
                              loop oz count; (* we go start again the event loop *)
                            end
                          end
                        end
                      end
                    end
                  end
            |false -> begin match Tree.go_up zip with
              |None -> Stdio.printf "\nErreur begin match go_up zip 2 (+)\n%!";
                loop oz count; (* we go start again the event loop *)
              |Some(Tree.TZ(context,node)) -> begin match Tree.get_node_data node with
                  |None -> Stdio.printf "\nErreur begin match Tree.get_node_data node 2 (+)\n%!";
                    loop oz count; (* we go start again the event loop *)
                  |Some(Split(direction,ratio),c) -> let zipn = Tree.change_up zip (Split(direction,(dec_ratio ratio)),c) in
                    begin match Tree.go_up zipn with
                      |None ->Stdio.printf "\nErreur begin match Tree.go_up zipn 2 (+)\n%!";
                        loop oz count; (* we go start again the event loop *)
                      |Some(Tree.TZ(_,nodes)) -> begin match direction with
                        |Vertical -> let oz = Tree.TZ(context,(Wm.update_coord (Wm.multiply_ratio_vertical_l (dec_ratio ratio) c) nodes)) in
                          Wm.draw_wmzipper inactive_color oz;
                          let oz = Tree.go_downr oz in begin match oz with
                            |None -> Stdio.printf "\nErreur begin match go_downr oz (+)\n%!";
                              loop oz count; (* we go start again the event loop *)
                            |Some(Tree.TZ(_,leaf)) -> begin match Tree.get_leaf_data leaf with
                              |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data leaf 3 (+)\n%!";
                                loop oz count; (* we go start again the event loop *)
                              |Some(window,coordonnee) -> Wm.draw_win window  coordonnee active_color;
                                loop oz count; (* we go start again the event loop *)
                              end
                            end
                        |Horizontal -> let oz = Tree.TZ(context,(Wm.update_coord (Wm.multiply_ratio_horizontal_l (dec_ratio ratio) c) nodes)) in
                          Wm.draw_wmzipper inactive_color oz;
                          let oz = Tree.go_downr oz in begin match oz with
                            |None -> Stdio.printf "\nErreur begin match oz (+)\n%!";
                              loop oz count; (* we go start again the event loop *)
                            |Some(Tree.TZ(_,leaf)) -> begin match Tree.get_leaf_data leaf with
                              |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data leaf 4 (+)\n%!";
                                loop oz count; (* we go start again the event loop *)
                              |Some(window,coordonnee) -> Wm.draw_win window  coordonnee active_color;
                                loop oz count; (* we go start again the event loop *)
                              end
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end


    | '-' ->
      Stdio.printf "\ndecrement size\n%!";
      begin
        begin match oz with
          |None -> Stdio.printf "\nAucune fenêtre de créée\n%!";
            loop oz count; (* we go start again the event loop *)
          |Some(Tree.TZ(Top,_)) -> Stdio.printf "\nAction impossible, vous avez créé qu'une seule fenêtre\n%!";
            loop oz count; (* we go start again the event loop *)
          |Some(zip) -> begin match Tree.is_left_context zip with
              |true -> begin match Tree.go_up zip with
                |None -> Stdio.printf "\nErreur begin match go_up zip\n%!";
                  loop oz count; (* we go start again the event loop *)
                |Some(Tree.TZ(context,node)) -> begin match Tree.get_node_data node with
                  |None -> Stdio.printf "\nErreur begin match Tree.get_node_data node 1 (-)\n%!";
                    loop oz count; (* we go start again the event loop *)
                  |Some(Split(direction,ratio),c) -> let zipn = Tree.change_up zip (Split(direction,dec_ratio ratio),c) in
                    begin match Tree.go_up zipn with
                      |None -> Stdio.printf "\nErreur Tree.go_up zipn 1\n%!";
                        loop oz count; (* we go start again the event loop *)
                      |Some(Tree.TZ(_,nodes)) -> begin match direction with
                        |Vertical -> let oz = Tree.TZ(context,(Wm.update_coord (Wm.multiply_ratio_vertical_l (dec_ratio ratio) c) nodes)) in
                          Wm.draw_wmzipper inactive_color oz;
                          let oz = Tree.go_down oz in
                          begin match oz with
                            |None -> Stdio.printf "\nErreur begin match go_down oz (-)\n%!";
                              loop oz count; (* we go start again the event loop *)
                            |Some(Tree.TZ(_,leaf)) -> begin match Tree.get_leaf_data leaf with
                                  |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data leaf 1 (-)\n%!";
                                    loop oz count; (* we go start again the event loop *)
                                  |Some(window,coordonnee) -> Wm.draw_win window  coordonnee active_color;
                                    loop oz count; (* we go start again the event loop *)
                                end
                              end
                        |Horizontal -> let oz = Tree.TZ(context,(Wm.update_coord (Wm.multiply_ratio_horizontal_l (dec_ratio ratio) c) nodes)) in
                          Wm.draw_wmzipper inactive_color oz;
                          let oz = Tree.go_down oz in
                          begin match oz with
                            |None -> Stdio.printf "\nErreur begin match oz (-)\n%!";
                              loop oz count; (* we go start again the event loop *)
                              |Some(Tree.TZ(_,leaf)) -> begin match Tree.get_leaf_data leaf with
                                |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data leaf 2 (-)\n%!";
                                  loop oz count; (* we go start again the event loop *)
                                |Some(window,coordonnee) -> Wm.draw_win window  coordonnee active_color;
                                  loop oz count; (* we go start again the event loop *)
                              end
                            end
                          end
                        end
                      end
                  end
            |false ->
              begin match Tree.go_up zip with
                |None -> Stdio.printf "\nErreur begin match go_up zip (-)\n%!";
                  loop oz count; (* we go start again the event loop *)
                |Some(Tree.TZ(context,node)) -> begin match Tree.get_node_data node with
                  |None -> Stdio.printf "\nErreur begin match Tree.get_node_data node 2 (-)\n%!";
                    loop oz count; (* we go start again the event loop *)
                    |Some(Split(direction,ratio),c) -> let zipn = Tree.change_up zip (Split(direction,(inc_ratio ratio)),c) in
                      begin match Tree.go_up zipn with
                        |None ->Stdio.printf "\nErreur begin match go_up zipn 2 (-)\n%!";
                          loop oz count; (* we go start again the event loop *)
                        |Some(Tree.TZ(_,nodes)) -> begin match direction with
                          |Vertical -> let oz = Tree.TZ(context,(Wm.update_coord (Wm.multiply_ratio_vertical_l (inc_ratio ratio) c) nodes)) in
                            Wm.draw_wmzipper inactive_color oz;
                            let oz = Tree.go_downr oz in begin match oz with
                              |None -> Stdio.printf "\nErreur begin match oz 1 (-)\n%!";
                                loop oz count; (* we go start again the event loop *)
                              |Some(Tree.TZ(_,leaf)) -> begin match Tree.get_leaf_data leaf with
                                |None -> Stdio.printf "\nErreur begin match Tree.get_leaf_data leaf 3 (-)\n%!";
                                  loop oz count; (* we go start again the event loop *)
                                |Some(window,coordonnee) -> Wm.draw_win window  coordonnee active_color;
                                  loop oz count; (* we go start again the event loop *)
                                end
                              end
                          |Horizontal -> let oz = Tree.TZ(context,(Wm.update_coord (Wm.multiply_ratio_horizontal_l (inc_ratio ratio) c) nodes)) in
                            Wm.draw_wmzipper inactive_color oz;
                            let oz = Tree.go_downr oz in begin match oz with
                              |None -> Stdio.printf "\nErreur begin match oz 2 (-)\n%!";
                                loop oz count; (* we go start again the event loop *)
                              |Some(Tree.TZ(_,leaf)) ->
                                begin match Tree.get_leaf_data leaf with
                                  |None -> Stdio.printf "\nErreur Tree.get_leaf_data leaf 4 (-)\n%!";
                                    loop oz count; (* we go start again the event loop *)
                                  |Some(window,coordonnee) -> Wm.draw_win window  coordonnee active_color;
                                    loop oz count; (* we go start again the event loop *)
                                end
                              end
                            end 
                          end 
                        end
                      end
                    end
                  end
                end

    | 'r' ->
      Stdio.printf "\nremove\n%!";
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count; (* we go start again the event loop *)
        |Some zip -> begin match zip with 
          |Tree.TZ(context,_) -> begin match context with
            |Top -> Graphics.clear_graph ();
              loop None (count-1); (* we go start again the event loop *)
            | _ -> begin match Tree.remove_leaf zip with
              |Some(Tree.TZ(c,v),(_,coord)) -> begin match v with
                |Tree.Leaf (w,_) -> let oz = Tree.TZ(c,Tree.Leaf(w, coord)) in
                  Wm.draw_win w coord active_color;
                  loop (Some(oz)) (count-1); (* we go start again the event loop *)
                |Tree.Node((Wm.Split(d,r),_),l1,l2) -> begin match d with
                  |Horizontal -> let tree =  Wm.update_coord (Wm.multiply_ratio_horizontal_l r coord) (Tree.Node((Wm.Split(d,r),coord),l1,l2)) in
                    let oz = Tree.TZ(c,tree) in
                    Wm.draw_wmzipper inactive_color oz;
                    begin match (Tree.next_leaf oz) with
                      |None -> Stdio.printf"\nErreur begin match Tree.next_leaf oz 1 (r)\n%!";
                        loop (Some(oz)) (count-1); (* we go start again the event loop *)
                      |Some(Tree.TZ(_,t)) -> Wm.draw_wmtree active_color t;
                        loop (Tree.next_leaf oz) (count-1); (* we go start again the event loop *)
                    end
                  |Vertical -> let tree =  Wm.update_coord (Wm.multiply_ratio_vertical_l r coord) (Tree.Node((Wm.Split(d,r),coord),l1,l2)) in
                    let oz = Tree.TZ(c,tree) in Wm.draw_wmzipper inactive_color oz;
                    begin match (Tree.next_leaf oz) with
                      |None -> Stdio.printf"\nErreur begin match Tree.next_leaf 2 (r)\n%!";
                        loop (Some(oz)) (count-1); (* we go start again the event loop *)
                      |Some(Tree.TZ(_,t)) -> Wm.draw_wmtree active_color t;
                        loop (Tree.next_leaf oz) (count-1); (* we go start again the event loop *)
                    end
                  end
                end
              |None -> Stdio.printf"\nErreur begin match Tree.remove_leaf zip (r)\n%!";
                loop oz count; (* we go start again the event loop *)
            end
          end
        end
      end

    | 'u' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = Color.select_couleur () in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (u)\n%!";
              loop oz count; end end end

    | '1' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = Color.blue in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (1)\n%!";
              loop oz count; end end end

    | '2' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = Color.red in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (2)\n%!";
              loop oz count; end end end

    | '3' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = Color.green in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (3)\n%!";
              loop oz count; end end end

    | '4' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = gray in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (4)\n%!";
              loop oz count; end end end
    
    | '5' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = cyan in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (5)\n%!";
              loop oz count; end end end

    | '6' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = magenta in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (6)\n%!";
              loop oz count; end end end

    | '7' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = teal in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (7)\n%!";
              loop oz count; end end end

    | '8' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = purple in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (8)\n%!";
              loop oz count; end end end

    | '9' ->
      begin match oz with
        |None -> Stdio.printf "\nIl n'y a aucune fenêtre\n%!";
          loop oz count;
        |Some zip -> begin match zip with
          |Tree.TZ(context,focus) -> begin match focus with
            |Tree.Leaf(Wm.Win(w,_),coord) -> let new_color = olive in Wm.draw_wmtree active_color (Tree.Leaf(Wm.Win(w,new_color),coord));
              loop (Some (Tree.TZ(context,Tree.Leaf(Wm.Win(w,new_color),coord)))) count;
            |Tree.Node(_,_,_) -> Stdio.printf "\nErreur begin match focus (9)\n%!";
              loop oz count; end end end

    | c ->
      printf "cannot process command '%c'\n%!" c;
      loop oz count
        
in

  
  try
    loop None count
  with
  | Stdlib.Exit -> ()


let () = main ()

