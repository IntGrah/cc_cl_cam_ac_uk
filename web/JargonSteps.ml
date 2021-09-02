open Slang
open Jargon

type node_tp =
  | H_INT
  | H_BOOL
  | H_UNIT
  | H_CI
  | H_HI
  | H_HEADER [@@deriving yojson]

type arrow_tp =
  | BLOCK
  | POINTER [@@deriving yojson]

type node = {
  id: string;
  label: string;
  parent: string;
  tp: node_tp;
  pointer: int option;
} [@@deriving yojson]

type edge = {
  source: string;
  target: string;
  label:  string;
  tp: arrow_tp;
} [@@deriving yojson]

type graph = node list * edge list [@@deriving yojson]

let string_of_heap_type tp = match tp with
  | HT_PAIR -> "Pair"
  | HT_INL -> "InL"
  | HT_INR -> "InR"
  | HT_CLOSURE -> "Closure"

let edges_of_heap (index : int) (hi : heap_item) : edge list  = match hi with
  | HEAP_INT _ -> []
  | HEAP_BOOL _ -> []
  | HEAP_UNIT -> []
  | HEAP_HI hi -> [{
    source = string_of_int index;
    target = string_of_int hi;
    label = "";
    tp = POINTER;
  }]
  | HEAP_CI _ -> []
  (* The heap headers size also includes the header so we use i - 1 *)
  | HEAP_HEADER (_, _) -> []

let index_box index = "[ " ^ index ^ " ]"

let node_of_heap_item index parent heap_item =
  let s_index = string_of_int index in
  let index_b = index_box s_index in
   match heap_item with
  | HEAP_INT i -> {
    id      = s_index;
    label   = index_b ^ " Int: " ^ (string_of_int i);
    tp      = H_INT;
    parent  = parent;
    pointer = None;
  }
  | HEAP_BOOL b -> {
    id      = s_index;
    label   = index_b ^ " Bool: " ^ string_of_bool b;
    tp      = H_BOOL;
    parent  = parent;
    pointer = None;
  }
  | HEAP_UNIT -> {
    id      = s_index;
    label   = index_b ^ "()";
    tp      = H_UNIT;
    parent  = parent;
    pointer = None;
  }
  | HEAP_HI i -> {
    id      = s_index;
    label   = index_b ^ " Heap Index: " ^ string_of_int i;
    tp      = H_HI;
    parent  = parent;
    pointer = Some i;
  }
  | HEAP_CI i -> {
    id      = s_index;
    label   = index_b ^ " Code Index: " ^ string_of_int i;
    tp      = H_CI;
    parent  = parent;
    pointer = Some i;
  }
  | HEAP_HEADER (_, ht) -> {
    id = s_index;
    label = index_b ^ " Heap Header: " ^ string_of_heap_type ht;
    tp    = H_HEADER;
    parent = parent;
    pointer = None;
  }

let rec node_list_of_heap_item_list_ index header n = function
  | [] -> []
  | (HEAP_HEADER (i, t))::heap_item_list ->
      node_of_heap_item index (if n > 0 then header else "") (HEAP_HEADER (i,t)) :: node_list_of_heap_item_list_ (index + 1) (string_of_int index) (i - 1) heap_item_list 
  | heap_item::heap_item_list -> node_of_heap_item index (if n > 0 then header else "") heap_item :: node_list_of_heap_item_list_ (index + 1) header (n - 1) heap_item_list

let node_list_of_heap_item_list heap_item_list =
  node_list_of_heap_item_list_ 0 "0" 0 heap_item_list

let graph_of_heap heap = (node_list_of_heap_item_list heap, List.flatten (List.mapi edges_of_heap heap))
type ret = {
    stack : string list;
    heap : string list;
    heap_graph: graph;
    sp : int;
    fp : int;  (* frame pointer *)
    cp : int;   (* code pointer  *)
    hp : int;   (* next free     *)
    status: string;
} [@@deriving yojson]

let string_lists_of_vm_state vm_state = match vm_state with {
    Jargon.stack = stack;
    stack_bound = _;
    code_bound = _;
    heap_bound = _;
    heap;
    code = _;
    sp;  (* stack pointer *)
    fp;  (* frame pointer *)
    cp;   (* code pointer  *)
    hp;   (* next free     *)
    status;
  } -> let heap_list = Array.to_list (Array.sub heap 0 hp) in {
    stack  = List.map Jargon.string_of_stack_item (Array.to_list (Array.sub stack 0 sp));
    heap   = List.map Jargon.string_of_heap_item heap_list;
    heap_graph = graph_of_heap heap_list;
    sp;
    fp;
    cp;
    hp;
    status = Jargon.string_of_status status;
  }

let string_list_of_code vm_state = List.map Jargon.string_of_instruction (Array.to_list vm_state.code)

let rec driver n vm =
  let state = string_lists_of_vm_state vm in
  state :: if vm.Jargon.status = Jargon.Running then driver (n+1) (step vm) else []

let steps exp =
  let c = compile exp in
  let vm = Jargon.first_frame (Jargon.initial_state c) in
  (string_list_of_code vm, driver 1 vm)
