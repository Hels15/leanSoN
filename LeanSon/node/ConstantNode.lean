import LeanSon.Node


open node

structure ConstantNode extends Node where
  value: Int


-- Sets control implicitly
def ConstantNode.NodeMk (nodel : Node) (control : Node) (value : Int) : M ConstantNode := do
  let uid := (← get).uniqueNodeId
  let newNode : ConstantNode := {toNode := {nid := uid, inputs := #[control], outputs := (#[] : Array Node) }, value := value}

  let updatedInputs := newNode.inputs.map (fun n => { n with outputs := n.outputs.push newNode })
  let newNode: ConstantNode := { newNode with toNode.inputs := updatedInputs }

  addNode2 newNode
  return newNode
