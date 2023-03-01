type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let example_tree = 
  Node(2,
    Node(1, Leaf, Leaf),
    Node(3, Leaf, Leaf)
    )

let rec size = function
  | Leaf -> 0
  | Node(_, l, r) -> 1 + size l + size r

let rec sum = function
  | Leaf -> 0
  | Node(v, l, r) -> v + sum l + sum r

(*-------------------------------------------------------------*)
(*binary tree with records using mutually recursive types*)
type 'a tree =
  | Leaf
  | Node of 'a node

and 'a node = {
  value: 'a;
  left: 'a tree;
  right: 'a tree
}

let t =
  Node {
    value = 2;
    left = Node {value = 1; left = Leaf; right = Leaf};
    right = Node {value = 3; left = Leaf; right = Leaf}
  }

(*mem is short for member*)
let rec mem x = function
| Leaf -> false
| Node {value; left; right} -> value = x || mem x left || mem x right

let rec preorder = function
  | Leaf -> []
  | Node {value; left; right} -> [value] @ preorder left @ preorder right
(*above preorder is quadratic time because of time complexity of @ operator is linear*)

let preorder_lin t =
  let rec pre_acc acc = function
    | Leaf -> acc
    | Node {value; left; right} -> value :: (pre_acc (pre_acc acc right) left)
  in pre_acc [] t
(*preorder_lin is linear time because for each node one cons operator is used which has constant time complexity*)