let red = true
let black = false

type ('key, 'value) node = {
  key : 'key;
  value: 'value;
  left: ('key, 'value) node option;
  right: ('key, 'value) node option;
  size: int;
  color: bool
}

let _is_red (node: ('key, 'value) node option) = 
  match node with 
    | None -> false
    | Some node -> node.color = red

type ('key, 'value) bst = {
  root : ('key, 'value) node option;
}

let empty_node key value color = {
  key=key;
  value=value;
  left=None;
  right=None;
  size=1;
  color=color;
}

let empty_bst() : ('key, 'value) bst = {
  root=None
}

let rec _get(node : ('key, 'value) node option)(key: 'key) : 'value option =
  match node with
    | None -> None
    | Some node -> 
      match node.key with
       | _key when _key = key -> Some node.value
       | _ -> let left_value = _get node.left key in 
              let right_value = _get node.right key in 
              match left_value, right_value with
                | None, Some value -> Some value
                | Some value, None -> Some value
                | None, None -> None
                | _ -> failwith "Duplicate keys are not allowed"

let rec get(bst : ('key, 'value) bst)(key: 'key) = _get bst.root key

let rec _min (node: ('key, 'value) node) = 
  match node.left with
    | None -> node
    | Some node -> _min (node)

let min (bst : ('key, 'value) bst) = 
  match bst.root with
    | None -> None
    | Some node -> (_min node).key

let rec _max (node: ('key, 'value) node) = 
  match node.right with 
    | None -> node
    | Some node -> _max node

let max (bst : ('key, 'value) bst) = 
  match bst.root with
    | None -> None
    | Some node -> Some (_max node).key

let has_key (bst : ('key, 'value) bst)(key: 'key) = 
  match get bst key with
    | None -> false
    | Some _ -> true

let size (bst : ('key, 'value) bst) = 
  match bst.root with
    | None -> 0
    | Some node -> node.size


let is_empty(bst : ('key, 'value) bst) = size bst == 0

(*the cons opeator has const time complexity, thus the following function is O(n)*)
let keys_inorder(bst : ('key, 'value) bst) = 
  let rec keys_inorder (list: 'key list) = function
    | None -> list
    | Some node -> 
      let right_list = keys_inorder list node.right in
      let middle_list = node.key :: right_list in 
      keys_inorder middle_list node.left in
  keys_inorder [] bst.root


let keys_preorder(bst : ('key, 'value) bst) = 
  let rec keys_inorder (list: 'key list) = function
    | None -> list
    | Some node -> 
      let right_list = keys_inorder list node.right in
      let middle_list = keys_inorder right_list node.left in 
      node.key :: middle_list in
  keys_inorder [] bst.root

let keys_postorder(bst : ('key, 'value) bst) = 
  let rec keys_inorder (list: 'key list) = function
    | None -> list
    | Some node -> 
      let right_list = node.key :: list in
      let middle_list = keys_inorder right_list node.right in 
      keys_inorder middle_list node.left in
  keys_inorder [] bst.root


(*this is a really bad implementation of the inorder function using the append operator
the append operator is O(a) with a being the number of elements in the first list, thus
this implementation is O((n/2)*n) -> O(n**2) -> this sucks big time 
*)
let keys_inorder_inefficient(bst : ('key, 'value) bst) = 
  let rec keys_inorder = function
    | None -> []
    | Some node -> [node.key] @ (keys_inorder node.left) @ (keys_inorder node.right) in
  keys_inorder bst.root

let _rotate_left (node: ('key, 'value) node) = 
  match node.right with
    | Some right when _is_red (Some right) -> {right with left = Some {node with right = right.left; color=red}; color = node.color}
    | _ -> failwith "invalid branch rl"

let _rotate_right (node: ('key, 'value) node) =
  match node.left with
    | Some left when _is_red (Some left) -> {left with right = Some {node with left = left.right; color = red}; color = node.color}
    | _ -> failwith "invalid branch rr"

let string_of_color = function
  | true -> "red"
  | false -> "black"

let string_of_node (node: (('key, 'value) node) option) = 
  let _string_of_node name key value color = name ^ "<" ^ string_of_int key ^ ", " ^ string_of_int value ^ ", " ^ string_of_color color ^ ">" in
  match node with 
  | None -> "None"
  | Some node -> match node.left, node.right with
    | None, None -> _string_of_node "EmptyNode" node.key node.value node.color
    | Some _, None -> _string_of_node "LeftFilledNode" node.key node.value node.color
    | None, Some _ -> _string_of_node "RightFilledNode" node.key node.value node.color
    | Some _, Some _ -> _string_of_node "FilledNode" node.key node.value node.color

let _flip_colors (node: ('key, 'value) node) =
  print_endline (string_of_node (Some node));
  print_endline (string_of_node node.left);
  print_endline (string_of_node node.right);
  match node.left, node.right with
    | Some left, Some right when not (_is_red (Some node)) && (_is_red (Some left)) && (_is_red (Some right)) -> 
      let new_left = {left with color = black} in 
      let new_right = {right with color = black} in 
      {node with color = red; left = Some new_left; right = Some new_right}
    | _ -> failwith "invalid branch fc"

let put key value (bst : ('key, 'value) bst) = 
  print_endline "sdfsf";
  print_endline (string_of_node bst.root);
  let rec _put key value = function
    | None -> if is_empty bst then empty_node key value black else empty_node key value red
    | Some node -> 
      let new_node = match node.key with
        | _key when _key > key -> {node with left = Some (_put key value node.left)}
        | _key when _key < key -> {node with right = Some (_put key value node.right)}
        | _ -> {node with value = value} in
      let left_rotated = if _is_red new_node.right && not (_is_red new_node.left) then 
        _rotate_left new_node else new_node in
      let right_rotated = match left_rotated.left with None -> left_rotated | Some left -> 
        if _is_red (Some left) && _is_red left.left then 
          _rotate_right left_rotated else left_rotated in
      if _is_red right_rotated.left && _is_red right_rotated.right then
        _flip_colors right_rotated else right_rotated in
  {root = Some (_put key value bst.root)}

let rec _remove_min (node: ('key, 'value) node) = 
  match node.left with
    | None -> node.right
    | Some _node ->
      let new_node = {node with left = _remove_min _node} in 
      Some {new_node with size = 1 + size {root=new_node.left} + size {root=new_node.right}}

let remove_min (bst : ('key, 'value) bst) = 
  match bst.root with
   | None -> bst
   | Some node -> {root=_remove_min node}

let rec _remove_max (node: ('key, 'value) node) = 
 match node.right with
   | None -> node.left
   | Some _node ->
     let new_node = {node with right = _remove_max _node} in 
     Some {new_node with size = 1 + size {root=new_node.left} + size {root=new_node.right}}

let remove_max (bst : ('key, 'value) bst) = 
 match bst.root with
  | None -> bst
  | Some node -> {root=_remove_max node}

(*hibbard deletion*)
let remove (bst : ('key, 'value) bst) key = 
  let rec _remove (node: ('key, 'value) node option) key = 
    match node with
      | None -> None (*node is none return none*)
      | Some {key=_key;value = _;right = None;left = _left;size = _;color=_} when _key = key -> _left (*node key matches key and right Node is None -> return left Node*)
      | Some {key=_key;value = _;right = _right;left = None;size = _;color=_} when _key = key -> _right (*node key matches key and left Node is None -> return right Node*)
      | Some node -> 
        let new_node = match node.key, node.right with
          | _key, _ when key < _key -> {node with left = _remove node.left key} (*key is smaller than key of node -> go left*)
          | _key, _ when key > _key -> {node with right = _remove node.right key} (*key is greater than key of node -> go right*)
          | _, Some right -> {(_min right) with right = (_remove_min right); left = node.left} (*key is equal to key of node and left and right child are not None*)
          | _ -> failwith "impossible branch" in
        Some {new_node with size = 1 + size({root=new_node.left}) + size({root=new_node.right})} in
  {root = _remove bst.root key}

let tree = empty_bst() |> put 3 3 |> put 5 5 |> put 7 7 |> put 9 9 |> put 11 11 |> put 2 2 |> put 1 1 |> put 100 100 |> put 101 101 |> keys_inorder