type ('key, 'value) node = {
  key : 'key;
  value: 'value;
  left: ('key, 'value) node option;
  right: ('key, 'value) node option;
  size: int;
}

type ('key, 'value) bst = {
  root : ('key, 'value) node option;
}

let empty_node key value = {
  key=key;
  value=value;
  left=None;
  right=None;
  size=1;
}

let empty_bst() : ('key, 'value) bst = {
  root=None
}

let treeOne = {
  root=Some{
    key=10;
    value=4;
    left=None;
    right=None;
    size=1;
  }
}

let treeTwo = {
  root=Some{
    key=10;
    value=4;
    left=Some{
      key=5;
      value=5;
      left=None;
      right=None;
      size=1;
    };
    right=Some{
      key=15;
      value=15;
      left=None;
      right=None;
      size=1;
    };
    size=3;
  }
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

let put key value (bst : ('key, 'value) bst) = 
  let rec _put key value = function
    | None -> empty_node key value
    | Some node -> 
      let new_node = match node.key with
        | _key when _key > key -> _put key value node.left
        | _key when _key < key -> _put key value node.right
        | _ -> {node with value = value} in
      {new_node with size = 1 + size {root=new_node.left} + size {root=new_node.right}} in
  {root = Some (_put key value bst.root)}
  



