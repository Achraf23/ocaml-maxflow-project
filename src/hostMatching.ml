open Graph
open Fulkerson
open Tools

type commodities =
  { 
    animals: bool ;

    smoking: bool ;

    paired_wsame_gender: bool }


type hacker =
  {
    id : int ;
    friday : bool ;
    saturday : bool ;
    boy : bool ;
    info_hack : commodities
  }

type host =
  {
    id : int ;
    guests_f : int ;
    guests_s : int ;
    boy : bool ;
    info_host : commodities
  }

exception InvalidInput of string

let get_bool (input: string) : bool =
  match input with
  | "yes" -> true
  | "no" -> false
  | _ -> raise (InvalidInput "Input must be either 'yes' or 'no'")

let create_host n arg = 
  let host = 
    {id= n;
    guests_f = int_of_string (List.nth arg 0);
    guests_s = int_of_string (List.nth arg 1);
    boy= get_bool (List.nth arg 2);
    info_host = {animals = get_bool (List.nth arg 3); 
    smoking = get_bool (List.nth arg 4); paired_wsame_gender = get_bool (List.nth arg 5)}} 
  in host

let create_hacker n arg = 
  let hacker = 
    {id= n;
    friday = get_bool (List.nth arg 0);
    saturday = get_bool (List.nth arg 1);
    boy= get_bool (List.nth arg 2);
    info_hack = {animals = get_bool (List.nth arg 3); 
    smoking = get_bool (List.nth arg 4); paired_wsame_gender = get_bool (List.nth arg 5)}} 
  in hacker

let from_file path =

  let infile = open_in path in

  (* Function to trim a list of strings *)
  let trim_string_list (lst: string list) =
     (List.map String.trim lst) in 

  let rec loop hosts hackers = 
    try
      let line = String.trim (input_line infile) in
        if line = "" then loop hosts hackers
        else 
          let tab = String.split_on_char ':' line in
          let id = int_of_string (List.nth tab 0) in
          let infos = trim_string_list (String.split_on_char ',' (List.nth tab 2)) in
          match String.trim (List.nth tab 1) with
          | "host" -> loop ( (create_host id infos)::hosts) hackers
          | "hacker" -> loop hosts ( (create_hacker id infos)::hackers) 
          | _ -> loop hosts hackers          
            
    with End_of_file -> 
      close_in infile;
      List.rev hosts, List.rev hackers  (* Reverse the lists to maintain order *)
  in
  
  loop [] []


let is_matching_info host hacker =
  let not_match = 
    (not(hacker.info_hack.animals) && host.info_host.animals) ||
    (not(hacker.info_hack.smoking) && host.info_host.smoking) ||
    (hacker.info_hack.paired_wsame_gender && host.info_host.paired_wsame_gender && hacker.boy!=host.boy) ||
    (hacker.info_hack.paired_wsame_gender && not(host.info_host.paired_wsame_gender) && hacker.boy!=host.boy) ||
    (not(hacker.info_hack.paired_wsame_gender) && host.info_host.paired_wsame_gender && hacker.boy!=host.boy)
    
  in if not_match then false else true 

let add_hackers_to_graph graph (hackers : hacker list) =
  let rec loop graph (hackers : hacker list) = 
    match hackers with
    | hacker::rest -> loop (new_node graph hacker.id) rest
    | _ -> graph
  in 
    loop graph hackers
  
    
let add_hosts_to_graph graph hosts =
  let rec loop graph hosts = 
    match hosts with
    | host::rest -> loop (new_node graph host.id) rest
    | _ -> graph
  in 
    loop graph hosts
     
let init_graph hackers hosts =
  let graph1  = add_hosts_to_graph empty_graph hosts in
  add_hackers_to_graph graph1 hackers

(*Returns 0, 1 or 2 depending whether the hacker stays on 
   firday or saturday or both *)
let get_hosting_days (h: hacker) : int =
  if h.friday && h.saturday then
    2
  else if h.friday || h.saturday then
    1
  else
    0

let add_matching_arcs graph hackers hosts = 

  let rec loop graph hackers hosts =
    match hosts with
    | host::rest -> 
      let filtered_hackers = List.filter (is_matching_info host) hackers in
      let next_graph = 
        List.fold_left (fun gr (hacker:hacker) -> new_arc gr {src=hacker.id; tgt=host.id; lbl=(get_hosting_days hacker)})
      graph filtered_hackers in loop next_graph hackers rest
    | _ -> graph
  in 
    loop graph hackers hosts

let create_bipartite_matching_graph hosts hackers = 
  (*Create a graph with nodes only*)
  let graph = init_graph hackers hosts in
  (*Add matching arcs between hackers and hosts*)
  let graph2 = add_matching_arcs graph hackers hosts in
  
  (*Add source node to the graph then link it with the hackers
    Weight equals the hosting days of the hacker *)
  let graph3 = new_node graph2 0 in 
  let graph4 = 
    List.fold_left (fun gr (hacker:hacker) -> new_arc gr {src=0; tgt = hacker.id; lbl=(get_hosting_days hacker)})
  graph3 hackers in
  
  (*Add destination node to the graph then link it with the hosts
    Weight equals the number of hackers that can be hosted in friday and saturday *)
  let id_dest_node = (List.length hackers + List.length hosts + 1) in 
  let graph5 = new_node graph4 id_dest_node in
  List.fold_left (fun gr host -> new_arc gr {src=host.id; tgt = id_dest_node; lbl=host.guests_f + host.guests_s})
  graph5 hosts
  

let host_matching hosts hackers =
  let graph = create_bipartite_matching_graph hosts hackers in 
  let flow_graph = create_flow_graph graph in
  run_ford_fulkerson graph flow_graph 0 (List.length hosts + List.length hackers + 1) 



(***** FOR DEBUGGING *****)

(* Function to print attributes of a single hacker *)
let print_hacker_attributes (h: hacker) : unit =
  Printf.printf "ID: %d\n" h.id;
  Printf.printf "Friday: %B\n" h.friday;
  Printf.printf "Saturday: %B\n" h.saturday;
  Printf.printf "Boy: %B\n" h.boy;
  Printf.printf "Animals: %B, Smoking: %B, Paired with same gender: %B\n"
    h.info_hack.animals h.info_hack.smoking h.info_hack.paired_wsame_gender;
  Printf.printf "----------------------\n"

(* Function to print attributes of a list of hackers *)
let print_hackers_attributes (hackers: hacker list) : unit =
  List.iter print_hacker_attributes hackers

(* Function to print attributes of a single host *)
let print_host_attributes (h: host) : unit =
  Printf.printf "ID: %d\n" h.id;
  Printf.printf "Guests on Friday: %d\n" h.guests_f;
  Printf.printf "Guests on Saturday: %d\n" h.guests_s;
  Printf.printf "Boy: %B\n" h.boy;
  Printf.printf "Animals: %B, Smoking: %B, Paired with same gender: %B\n"
    h.info_host.animals h.info_host.smoking h.info_host.paired_wsame_gender;
  Printf.printf "----------------------\n"

(* Function to print attributes of a list of hosts *)
let print_hosts_attributes (hosts: host list) : unit =
  List.iter print_host_attributes hosts