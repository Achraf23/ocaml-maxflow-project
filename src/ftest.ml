 (* open Gfile *)
(* open Tools *)
(*open Fulkerson*)
(* open Graph  *)
open HostMatching
(* open BellmanFord *)
    
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
  
  let infile = Sys.argv.(1) in
  (* and outfile = Sys.argv.(4) in *)
  
  (* These command-line arguments are not used for the moment. *)
  (* and source = int_of_string Sys.argv.(2)
  and target = int_of_string Sys.argv.(3)
  in

  Printf.printf "Source: %d\n" source ;
  Printf.printf "Target: %d\n" target ; *)

  (* Open file *)
  (*HACKER TESTS*)
  let hosts,hackers = from_data_file infile in
  host_matching hosts hackers "test" 
  
  (* write_file outfile (gmap matching string_of_int)  *)


  
  (*BELLMAN FORD TESTS*)
  (* let rec aff nodes =
    Printf.printf "printed nodes:\n" ;
    match nodes with
    | [] -> ()
    | x::rest -> Printf.printf "%d " x ; aff rest in

  let graph = from_file infile in
  let float_graph = gmap graph float_of_string in 
  aff (find_shortest_path float_graph 0 5) *)