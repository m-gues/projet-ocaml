open Graph
open Gfile
open Tools

(* returns the initial residual graph of gr *)
let residual_graph gr = let aux acu id1 id2 label = new_arc acu id2 id1 0
  in e_fold gr aux gr

(* finds a path on gr between two nodes n1 and n2, returns the path as a list of arcs *)
let find_path gr n1 n2 =
  let rec aux acu id1 successeurs =
    if id1=n2 then
      Some (List.rev acu)
    else
      match successeurs with
      |[] -> None
      |(_, 0)::rest -> aux acu id1 rest 
      |(id2,label)::rest ->
        if ((List.mem (id1, id2, label) acu)||(label=0)) then None
          else
            (match aux ((id1, id2, label)::acu) id2 (out_arcs gr id2)  with
             |None -> aux acu id1 rest
             |Some path -> Some path
            )
    in aux [] n1 (out_arcs gr n1)

(*Returns max additionnal capacity of a path (list of arcs), -1 means infinite capacity (ie within the same node)*)
let rec max_add_capacity_path path = match path with
  |[]-> -1
  |(id1, id2, label)::rest-> let cap_rest = max_add_capacity_path rest in
                             if (cap_rest !=(-1) && cap_rest<label) then cap_rest else label
                          
(*Augments flow of a path by a on a residual graph*)
let rec augment_flow gr path a = match path with
  |[] -> gr
  |(id1, id2,_)::rest -> add_arc (add_arc (augment_flow gr rest a) id1 id2 a) id2 id1 (-a)

(* returns the final flow graph, given the original graph gr_init and the residual graph gr_res*)
  let flow_graph gr_init gr_res =
    let aux gr2 acu id1 id2 label =
      match (find_arc gr2 id1 id2) with
      |None -> failwith "erreur construction graphe"
      |Some a -> new_arc acu id1 id2 (label-a)
    in e_fold gr_init (aux gr_res) (clone_nodes gr_init)
  

(* executes the Ford-Fulkerson algorithm on gr, returns a graph with the maximum flow on each arc *)
  let ford_fulkerson gr_init origin dest =
    let rec aux gr_res = 
      match find_path gr_res origin dest with
      |None -> flow_graph gr_init gr_res
      |Some path -> (match (max_add_capacity_path path) with
                     |(-1) -> failwith "origin and destination are the same node"
                     |cap -> aux (augment_flow gr_res path cap)
                    )
    in aux (residual_graph gr_init)


(* Execution on graph1 from node 0 to node 5 *)
 (* let () = let gr = gmap (from_file "./graph1") int_of_string
         in export "./graphtest" (gmap (ford_fulkerson gr 0 5) string_of_int)*)




    
         
