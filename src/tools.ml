open Graph
open Gfile

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = 
  e_fold gr (fun g arc ->  (new_arc g {src=arc.src; tgt=arc.tgt; lbl=f arc.lbl})) (clone_nodes gr)

let add_arc graph id1 id2 n = 
  let arc = find_arc graph id1 id2 in
  match arc with
  | None -> new_arc graph {src=id1; tgt=id2; lbl=n}
  | Some arc -> new_arc graph {src=arc.src; tgt=arc.tgt; lbl=arc.lbl+n}

let create_flow_graph graph = gmap graph (fun _ -> 0)
  
let update_flow_graph original_flow_graph condition backward_condition increment =
  e_fold
    original_flow_graph
    (fun acc_graph arc ->
      let modified_arc =
        if condition arc then
          { arc with lbl = arc.lbl + increment }
        else if backward_condition arc then
          { arc with lbl = arc.lbl - increment }
        else
          arc
      in
      new_arc acc_graph modified_arc)
    original_flow_graph   (* 'b in e_fold is int graph *)

let rec check_if_arc_is_in_path arc path =
  match path with
  | [] -> false
  | _ :: [] -> false 
  | node1 :: node2 :: rest ->
    if (node1, node2) = (arc.src, arc.tgt) then
      true
    else
      check_if_arc_is_in_path arc (node2 :: rest)

let rec check_if_backward_arc_is_in_path arc path =
  match path with
  | [] -> false
  | _ :: [] -> false 
  | node1 :: node2 :: rest ->
    if (node2, node1) = (arc.src, arc.tgt) then
      true
    else
      check_if_backward_arc_is_in_path arc (node2 :: rest)

let rec find_max_flow_on_path_bleh graph flow_graph path =
  match path with
  | [] -> None
  | _ :: [] -> None 
  | node1 :: node2 :: rest ->
    (*(match find_arc graph node1 node2 with
     | Some arc -> Printf.printf "Arc1: src: %d tgt: %d lbl: %d\n" arc.src arc.tgt arc.lbl
     | None -> Printf.printf "Arc1: None\n");
    (match find_arc flow_graph node1 node2 with
     | Some arc -> Printf.printf "Arc2: src: %d tgt: %d lbl: %d\n" arc.src arc.tgt arc.lbl
     | None -> Printf.printf "Arc2: None\n");
    (match find_arc graph node2 node1 with
     | Some arc -> Printf.printf "Arc1 rev: src: %d tgt: %d lbl: %d\n" arc.src arc.tgt arc.lbl
     | None -> Printf.printf "Arc1: None\n");
    (match find_arc flow_graph node2 node1 with
     | Some arc -> Printf.printf "Arc2 rev: src: %d tgt: %d lbl: %d\n" arc.src arc.tgt arc.lbl
     | None -> Printf.printf "Arc1: None\n");*)
    let arc1_opt = find_arc graph node1 node2 in
    let arc2_opt = find_arc flow_graph node1 node2 in
    let arc1_rev_opt = find_arc graph node2 node1 in
    let arc2_rev_opt = find_arc flow_graph node2 node1 in
    match (arc1_opt, arc2_opt, arc1_rev_opt, arc2_rev_opt) with
    | (Some arc1, Some arc2, None, None) ->
      Printf.printf "Case 1 \n";
      let rest_difference_opt = find_max_flow_on_path_bleh graph flow_graph (node2 :: rest) in
      begin
        match rest_difference_opt with
        | Some rest_difference -> Some (min (arc1.lbl - arc2.lbl) rest_difference)
        | None -> Some (arc1.lbl - arc2.lbl) (* we've reached the end *)
      end
    | (None, None, Some _, Some arc2_rev) ->
      Printf.printf "Case 2 \n";
      let rest_difference_opt = find_max_flow_on_path_bleh graph flow_graph (node2 :: rest) in
      begin
        match rest_difference_opt with
        | Some rest_difference -> Some (min (arc2_rev.lbl) rest_difference)
        | None -> Some (arc2_rev.lbl) (* we've reached the end *)
      end
    | (Some arc1, Some arc2, Some _, Some arc2_rev) ->
      Printf.printf "Case 3 \n";
      let rest_difference_opt = find_max_flow_on_path_bleh graph flow_graph (node2 :: rest) in
      begin
        match rest_difference_opt with
        | Some rest_difference -> Some (min (arc1.lbl - arc2.lbl + arc2_rev.lbl) rest_difference)
        | None -> Some (arc1.lbl - arc2.lbl + arc2_rev.lbl) (* we've reached the end *)
      end
    | _ -> None

  


let find_max_flow_on_path graph flow_graph path =
  let result = find_max_flow_on_path_bleh graph flow_graph path in
  match result with
  | Some value -> 
    Printf.printf "Result: %d\n" value; 
    let string_flow_graph = gmap flow_graph (fun x -> string_of_int x) in
    if value = 2 then write_file "res2.txt" string_flow_graph
    else write_file "resmin2.txt" string_flow_graph;
    result
  | None -> 
    Printf.printf "No result\n"; 
    result




  

    