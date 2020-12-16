open Graph
open Gfile
open Tools

val residual_graph : int graph -> int graph
val find_path : int graph -> id -> id -> (id*id*int) list option
val max_add_capacity_path : (id*id*int) list -> int
val augment_flow : int graph -> (id*id*int) list -> int -> int graph
val flow_graph : int graph -> int graph -> int graph
val ford_fulkerson : int graph -> id -> id -> int graph
