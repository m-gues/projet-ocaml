open Graph
open Gfile

(* returns a new graph with the same nodes as gr but no arcs*)
let clone_nodes gr = n_fold gr new_node empty_graph

(* map all arcs of gr by function f *)
let gmap gr f = let aux acu id1 id2 label = new_arc acu id1 id2 (f label)
  in e_fold gr aux (clone_nodes gr)


(* add n to the value of the arc between n1 and n2, create if it does not exist*)
let add_arc gr n1 n2 label = match find_arc gr n1 n2 with
  |None-> new_arc gr n1 n2 label;
  |Some a -> new_arc gr n1 n2 (label+a)

(*This was for testing purposes*)
(*let () = export "./graph3" (from_file "./graph3.txt")*)
  
