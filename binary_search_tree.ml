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