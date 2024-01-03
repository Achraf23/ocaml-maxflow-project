(* open Graph *)
(* open Printf *)


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
    needs : commodities
  }

type host =
  {
    id : int ;
    guests_f : int ;
    guests_s : int ;
    boy : bool ;
    needs : commodities
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
    guests_f = 0;
    guests_s = 0;
    boy= get_bool (String.trim (List.nth arg 2));
    needs = {animals = true; smoking = true; paired_wsame_gender = true}} 
  in host

let create_hacker n arg = 
  let hacker = 
    {id= n;
    friday = true;
    saturday = true;
    boy= get_bool  (String.trim (List.nth arg 2) );
    needs = {animals = true; smoking = true; paired_wsame_gender = true}} 
  in hacker

let from_file path =

  let infile = open_in path in

  let rec loop hosts hackers = 
    try
      let line = String.trim (input_line infile) in
      (* Printf.printf "line: %s\n" line ; *)
        if line = "" then loop hosts hackers
        else 
          let tab = String.split_on_char ':' line in
            match String.trim (List.nth tab 1) with
            | "host" -> 
              let h = create_host (int_of_string (List.nth tab 0)) (String.split_on_char ',' (List.nth tab 2)) in
              loop (h::hosts) hackers
            | "hacker" -> 
              let h = create_hacker (int_of_string (List.nth tab 0)) (String.split_on_char ',' (List.nth tab 2)) in
              loop hosts (h::hackers)
            | _ -> loop hosts hackers
          
            
    with End_of_file -> Printf.printf "end of file"
  in
  
  loop [] []
