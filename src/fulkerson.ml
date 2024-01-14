open Graph
open Tools
open BellmanFord
(* open Gfile *)

let rec _aff nodes =
  match nodes with
  | [] -> ()
  | x::rest -> Printf.printf "%d " x ; _aff rest 

let successors n graph = 
  let successors = out_arcs graph n in
  List.map (fun arc -> arc.tgt) successors

(*idList represents nodes that have been visited*)
let rec find_path_aux graph idList src tgt = 
  if src = tgt then
    [src]
  else
    let neighbors = successors src graph in
    let rec loop successors =
      match successors with
      | [] -> [] (* pas de chemin *)
      | n1 :: restNeighbors ->
        if List.mem n1 idList then
          loop restNeighbors
        else
          (*Find path for first neighbor*)
          let path = find_path_aux graph (n1 :: idList) n1 tgt in
          (* Printf.printf "%d : idList= " src; aff idList ; *)
          (* Printf.printf "\n"; *)
          match path with
          | [] -> loop restNeighbors (*No path so try to find path from other neighbors*)
          | _ ->
            src :: path (*Return found path from n1*)
      in
      loop neighbors
  
let find_path graph src tgt =
  (* Printf.printf "Starting to find path\n"; *)
  find_path_aux graph [src] src tgt 
  
let build_difference_graph origin_graph flow_graph = 
  let build = clone_nodes origin_graph in
  e_fold origin_graph 
  (fun graph arc -> 
    (*find matching arc in flow grah*)
    let arc_f = find_arc flow_graph arc.src arc.tgt in
    match arc_f with
    | Some arc_flow -> 
      let diff = arc.lbl- arc_flow.lbl in
        (match diff with
        (*calculate weight difference between original graph and flow graph*)
        | 0 -> new_arc graph {src=arc.tgt; tgt=arc.src; lbl=arc.lbl}
        (*diff=0 --> forward arc*)
        | w when w=arc.lbl -> new_arc graph {src=arc.src; tgt=arc.tgt; lbl=w}
        (*2 flow arcs if flow val < capacity and not equal to zero*)
        | _ -> let ajout = new_arc graph {src=arc.src; tgt=arc.tgt; lbl=diff} in
        new_arc ajout {src=arc.tgt; tgt=arc.src; lbl=arc_flow.lbl})
    | None -> empty_graph 
    ) 
    build

exception Difference_graph_error

let rec run_ford_fulkerson graph flow_graph src tgt =
  (* Printf.printf "Starting Ford Fulkerson iteration\n";  *)
  let difference_graph = build_difference_graph graph flow_graph in
  if difference_graph = empty_graph then
    raise Difference_graph_error
  else
    (* write_file "outfileTest" (gmap difference_graph string_of_int); *)
    let path : int list = find_path difference_graph src tgt in
    match path with
    | [] -> flow_graph (*Return last flow graph because can't maximize the flow further*)
    | _ ->
      (* Printf.printf "Path: [%s]\n" (String.concat "; " (List.map string_of_int path)); *)
      let new_flow_optional : int option = find_max_flow_on_path graph flow_graph path in
      let new_flow : int = Option.value new_flow_optional ~default:0 in
      (* Printf.printf "Flow: %d\n" new_flow; *)
      let updated_flow_graph = update_flow_graph flow_graph (fun arc -> check_if_arc_is_in_path arc path) (fun arc -> check_if_backward_arc_is_in_path arc path) new_flow in
      run_ford_fulkerson graph updated_flow_graph src tgt 

(*For max flow min cost problem :
   Create 2 graphs from a graph that contains capacity and costs in arc labels*)
let separate_graphs graph = 
  let init_graph = clone_nodes graph in
  let cost_graph = 
    e_fold graph 
    (fun acu_graph arc -> 
      let (_,cost) = arc.lbl in
      new_arc acu_graph {src=arc.src; tgt=arc.tgt; lbl=cost}) init_graph in
  let capacity_graph = 
    e_fold graph 
    (fun acu_graph arc -> 
      let (capacity,_) = arc.lbl in
      new_arc acu_graph {src=arc.src; tgt=arc.tgt; lbl=capacity}) init_graph 
    in capacity_graph,cost_graph


let build_difference_graph_costs origin_graph flow_graph cost_graph = 
  let build = clone_nodes origin_graph in
  e_fold origin_graph 
  (fun graph arc -> 
    (*find matching arc in flow grah*)
    let arc_c = find_arc cost_graph arc.src arc.tgt in
    let arc_f = find_arc flow_graph arc.src arc.tgt in
    match arc_f,arc_c with
    | Some arc_flow,Some arc_cost -> 
      let diff = arc.lbl- arc_flow.lbl in
        (match diff with
        (*calculate weight difference between original graph and flow graph*)
        | 0 -> new_arc graph {src=arc.tgt; tgt=arc.src; lbl=arc_cost.lbl}
        (*diff=0 --> forward arc*)
        | w when w=arc.lbl -> new_arc graph {src=arc.src; tgt=arc.tgt; lbl=w}
        (*2 flow arcs if flow val < capacity and not equal to zero*)
        | _ -> let ajout = new_arc graph {src=arc.src; tgt=arc.tgt; lbl=arc_cost.lbl} in
        new_arc ajout {src=arc.tgt; tgt=arc.src; lbl= -arc_cost.lbl})
    | (_,_) -> empty_graph 
    ) 
    build


let rec max_flow_min_cost_algo (graph : (id * id) graph) flow_graph src tgt =

  let capacity_graph,cost_graph = separate_graphs graph in   
  (* write_file "graph.txt" (gmap capacity_graph string_of_int);  *)
  Printf.printf "Starting Ford Fulkerson iteration\n"; 
  let difference_graph = build_difference_graph_costs capacity_graph flow_graph cost_graph in
  if difference_graph = empty_graph then
    raise Difference_graph_error
  else
    (* write_file "ecart2.txt" (gmap difference_graph string_of_int); *)
    let path : int list = find_shortest_path (gmap difference_graph float_of_int) src tgt in
    (* _aff path ; *)
    match path with
    | [] -> flow_graph (*Return last flow graph because can't maximize the flow further*)
    | _ ->
      (* Printf.printf "Path: [%s]\n" (String.concat "; " (List.map string_of_int path)); *)
      let new_flow_optional : int option = find_max_flow_on_path capacity_graph flow_graph path in
      let new_flow : int = Option.value new_flow_optional ~default:0 in
      Printf.printf "Flow: %d\n" new_flow;
      let updated_flow_graph = update_flow_graph flow_graph (fun arc -> check_if_arc_is_in_path arc path) (fun arc -> check_if_backward_arc_is_in_path arc path) new_flow in
      (* write_file "flow.txt" (gmap updated_flow_graph string_of_int); *)
      max_flow_min_cost_algo graph updated_flow_graph src tgt 

let max_flow_min_cost graph src tgt = 
  let capacity_graph,_ = separate_graphs graph in
  let flow_graph = create_flow_graph capacity_graph in
  max_flow_min_cost_algo graph flow_graph src tgt 
  

