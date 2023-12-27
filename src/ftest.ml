open Gfile
open Tools
open Fulkerson
open Graph
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and source = int_of_string Sys.argv.(2)
  and target = int_of_string Sys.argv.(3)
  in

  Printf.printf "Source: %d\n" source ;
  Printf.printf "Target: %d\n" target ;

  (* Open file *)
  let string_graph = from_file infile in
  let graph = gmap string_graph (fun x -> int_of_string x) in

  if not (node_exists graph source) then begin
    Printf.printf "Error: Source node %d does not exist in the graph.\n" source;
    exit 1;
  end;

  if not (node_exists graph target) then begin
    Printf.printf "Error: Target node %d does not exist in the graph.\n" target;
    exit 1;
  end;

  let flow_graph = create_flow_graph graph in

  (*match find_max_flow_on_path graph flow_graph [0; 1; 2] with
  | Some max_flow -> Printf.printf "Max flow: %d\n" max_flow
  | None -> Printf.printf "Max flow: 0\n";*)

  let flow_graph = run_ford_fulkerson graph flow_graph source target in

  Printf.printf "Ford Fulkerson is done\n";

  let string_flow_graph = gmap flow_graph (fun x -> string_of_int x) in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile string_flow_graph in

  ()

