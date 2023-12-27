open Graph
open Tools
open Gfile

let rec aff nodes =
  match nodes with
  | [] -> ()
  | x::rest -> Printf.printf "%d " x ; aff rest 

let successors n graph = 
  let successors = out_arcs graph n in
  List.map (fun arc -> arc.tgt) successors

let rec find_path_aux graph idList src tgt =
  if src = tgt then
    [src]
  else
    let neighbors = successors src graph in
    let rec loop rest =
      match rest with
      | [] -> [] (* pas de chemin *)
      | n1 :: rest' ->
        if List.mem n1 idList then
          loop rest'
        else
          let path = find_path_aux graph (n1 :: idList) n1 tgt in
          Printf.printf "%d : idList= " src; aff idList ;
          Printf.printf "\n";
          match path with
          | [] -> loop rest'
          | _ ->
            src :: path
      in
      loop neighbors
  
let find_path graph src tgt =
  Printf.printf "Starting to find path\n";
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
  Printf.printf "Starting Ford Fulkerson iteration\n";
  let difference_graph = build_difference_graph graph flow_graph in
  if difference_graph = empty_graph then
    raise Difference_graph_error
  else
    write_file "outfileTest" (gmap difference_graph string_of_int);
    let path : int list = find_path difference_graph src tgt in
    match path with
    | [] -> flow_graph
    | _ ->
      Printf.printf "Path: [%s]\n" (String.concat "; " (List.map string_of_int path));
      let new_flow_optional : int option = find_max_flow_on_path graph flow_graph path in
      let new_flow : int = Option.value new_flow_optional ~default:0 in
      Printf.printf "Flow: %d\n" new_flow;
      let updated_flow_graph = update_flow_graph flow_graph (fun arc -> check_if_arc_is_in_path arc path) (fun arc -> check_if_backward_arc_is_in_path arc path) new_flow in
      run_ford_fulkerson graph updated_flow_graph src tgt

  

