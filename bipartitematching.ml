open Graph
open Gfile
open Tools
open Fordfulkerson
open Printf
open String

(* Reads a line with a book*)
let read_book graph line =
  try Scanf.sscanf line "b %d %s" (fun n s -> new_arc (new_node graph n) n 99 1)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with an attendee*)
let read_attendee graph line =
  let process_attendee gr1 n1 s1 =
    let rec aux gr2 list = match list with
      |[]-> gr2
      |book::rest-> aux (new_arc gr2 n1 (int_of_string book) 1) rest
    in aux gr1 (String.split_on_char ',' s1)
  in
  try Scanf.sscanf line "a %d %s : %s %s" (fun n s1 l s2 -> process_attendee (new_arc (new_node graph n) 0 n 1) n l)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

(* Take a path to a bipartite matching file and returns the corresponding bipartite matching graph*)
let bmfile_to_bmgraph path = 

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, graph2) =
        (* Ignore empty lines *)
        if line = "" then (n, graph)

        (* The first character of a line determines its content : Attendee or book *)
        else match line.[0] with
          | 'b' -> (n+1, read_book graph line)
          | 'a' -> (n, read_attendee graph line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line)
      in      
      loop n2 graph2
    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop 0 (new_node (new_node empty_graph 0) 99)

     in
  
  close_in infile ;
  final_graph
  
(* Bipartite match the given graph, returns a graph without the null arcs for lisibility*)
let bipartite_matching gr =
  let remove_null_arcs gr1 =
    let aux acu id1 id2 label = match label with
      |0-> acu
      |_-> new_arc acu id1 id2 label
    in e_fold gr1 aux (clone_nodes gr1)
  in remove_null_arcs(ford_fulkerson gr 0 99)

(*Execution on the book club file*)
(*let () = let gr = (bmfile_to_bmgraph "./bmfile")
  in export "./graphtest" (gmap (bipartite_matching gr) string_of_int)*)
