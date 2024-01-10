open Graph

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


val from_file : string -> host list * hacker list 

val host_matching_graph : host list -> hacker list -> id graph

val host_matching : host list -> hacker list -> string -> unit