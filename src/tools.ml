open Graph

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
  let find_reverse_arc graph arc =
    find_arc graph arc.tgt arc.src
  in
  e_fold
    original_flow_graph
    (fun acc_graph arc ->
      let reverse_arc_opt = find_reverse_arc original_flow_graph arc in
      let modified_arc =
        if condition arc then { arc with lbl = arc.lbl + increment }
        else if backward_condition arc then
          match reverse_arc_opt with
          | Some reverse_arc when condition reverse_arc -> arc
          | _ -> { arc with lbl = arc.lbl - increment }
        else arc
      in
      new_arc acc_graph modified_arc)
    original_flow_graph

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

let rec find_max_flow_on_path graph flow_graph path =
  match path with
  | [] -> None
  | _ :: [] -> None 
  | node1 :: node2 :: rest ->
    let arc1_opt = find_arc graph node1 node2 in
    let arc2_opt = find_arc flow_graph node1 node2 in
    let arc1_rev_opt = find_arc graph node2 node1 in
    let arc2_rev_opt = find_arc flow_graph node2 node1 in
    match (arc1_opt, arc2_opt, arc1_rev_opt, arc2_rev_opt) with
    | (Some arc1, Some arc2, None, None) ->
      let rest_difference_opt = find_max_flow_on_path graph flow_graph (node2 :: rest) in
      begin
        match rest_difference_opt with
        | Some rest_difference -> Some (min (arc1.lbl - arc2.lbl) rest_difference)
        | None -> Some (arc1.lbl - arc2.lbl)
      end
    | (None, None, Some _, Some arc2_rev) ->
      let rest_difference_opt = find_max_flow_on_path graph flow_graph (node2 :: rest) in
      begin
        match rest_difference_opt with
        | Some rest_difference -> Some (min arc2_rev.lbl rest_difference)
        | None -> Some arc2_rev.lbl
      end
    | (Some arc1, Some arc2, Some _, Some _) ->
      let rest_difference_opt = find_max_flow_on_path graph flow_graph (node2 :: rest) in
      begin
        match rest_difference_opt with
        | Some rest_difference ->
          if (arc1.lbl - arc2.lbl) > 0 then
            Some (min (arc1.lbl - arc2.lbl) rest_difference)
          else
            Some (min arc1.lbl rest_difference)
        | None ->
          if (arc1.lbl - arc2.lbl) > 0 then
            Some (arc1.lbl - arc2.lbl)
          else
            Some arc1.lbl
      end
    | _ -> None
      

  


  





  

    