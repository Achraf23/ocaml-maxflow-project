open Graph

let rec aff nodes =
  Printf.printf "printed nodes:\n" ;
  match nodes with
  | [] -> ()
  | x::rest -> Printf.printf "%d " x ; aff rest 

let successors n graph = 
  let successors = out_arcs graph n in
  List.map (fun arc -> arc.tgt) successors

let predecessors n graph = 
  let is_predecessor elt =
    let neighbors = successors elt graph in
    if List.mem n neighbors then true else false
  in 
  n_fold graph (fun predecessors elt -> if is_predecessor elt then elt::predecessors else predecessors) []


  type label =
  { 
    id: id ;

    cost: float ;

    father: id }

let get_cost n labels = 
  let label = List.hd (List.filter (fun label -> n=label.id) labels) in label.cost



let init_bellman graph s = 
  n_fold graph (fun labels n -> 
    if n!=s then {id=n; cost=Float.infinity; father= -1}::labels
    else {id=n; cost=Float.zero; father= -1}::labels ) [] 

let update_label labels n cost father = 
  let restLabels = List.filter (fun label -> n!=label.id) labels in 
  {id=n; cost=cost; father=father}::restLabels


exception Arc_Not_Found

let run_bellman_ford graph =

  let labels_initialized = init_bellman graph 0 in
  
  let update_node_cost node labels =

    let predecessors = predecessors node graph in
      let rec loop_predecessors node predecessors labels_aux = 
        match predecessors with
        | [] -> labels_aux
        | neighbor::rest -> 
          let arc_opt = find_arc graph neighbor node in
          match arc_opt with
          | Some arc ->
              let cost_node = get_cost node labels_aux in
              let new_cost = min cost_node (Float.add arc.lbl (get_cost neighbor labels_aux)) in
              if new_cost != cost_node then
                let labels_updated = update_label labels_aux node new_cost neighbor in 
                loop_predecessors node rest labels_updated
              else loop_predecessors node rest labels_aux 
          | None -> loop_predecessors node rest labels_aux

      in loop_predecessors node predecessors labels
    in
    let length_nodes = n_fold graph (fun acu _id -> acu+1) 0 in 
    
    let rec loop_algo k current_labels= 
      if k < length_nodes then
        let new_labels = 
          n_fold graph (fun labels_acu node -> if node!=0 then update_node_cost node labels_acu else labels_acu) current_labels
        in loop_algo (k+1) new_labels 
      else current_labels in 
    loop_algo 0 labels_initialized

  (*Finds path to the target from the origin node from which the algorithm has been launched*)
let find_shortest_path graph tgt =
  let labels = run_bellman_ford graph in
  let rec recover_path node path = 
    let label = List.hd (List.filter (fun n -> n.id=node) labels) in
    if label.father = 0 then 
      0::path
    else recover_path label.father (label.father::path) 
  in recover_path tgt [tgt]


(*FOR DEBUGGING*)

(* Function to print a label *)
let print_label l =
  Printf.printf "ID: %d, Cost: %f, Father: %d\n" l.id l.cost l.father

(* Function to print a list of labels *)
let print_labels_list labels =
  List.iter print_label labels


