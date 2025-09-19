

--A private Global Static mutable counter, for unique node id generation.
-- To make the compiler multi-threaded, this field will have to move into a TLS.
-- Starting with value 1, to avoid bugs confusing node ID 0 with uninitialized values.

-- Represents an index into the arena
structure NodeRef where
  nid: Nat
deriving Inhabited, Repr

inductive NodeData where
  | constantl (value: Int64)
  | nullData
deriving Inhabited, Repr

structure Node where
ref: NodeRef
inputs:  Array  NodeRef := #[]
outputs: Array  NodeRef := #[]
data: NodeData
deriving Inhabited, Repr

-- Node arena
structure ManyNodes where
    uniqueNodeId : Nat
    allNodes : Array Node

abbrev M := StateRefT ManyNodes IO


def updateNode (idx : Nat) (f : Node → Node) : M Unit := do
  modify fun s =>
    { s with allNodes := s.allNodes.modify idx f }

def updateInputsOutputsNid (inputs : Array NodeRef) (newNode : Node) : M Unit := do
  for ref in inputs do
    updateNode ref.nid (fun n => { n with outputs := n.outputs.push newNode.ref })

-- Each node has a unique dense Node ID within a compilation context
-- The ID is useful for debugging, for using as an offset in a bitvector,
-- as well as for computing equality of nodes (to be implemented later).
--

namespace node
--        ///////// ARENA    ///////////

-- Add a custom node to the arena
def addNode2 (newNode : Node) : M Unit := do
  -- update the arena: increment unique ID and store the new node
  modify fun arena =>
    { arena with
      uniqueNodeId := arena.uniqueNodeId + 1,
      allNodes     := arena.allNodes.push newNode }


--        ///////// ARENA    ///////////

-- Create a new node and update outputs of input nodes
def NodeMK (inputs : Array NodeRef := #[]) : M Node := do
  let uid := (← get).uniqueNodeId
  let ref: NodeRef := {nid := uid}
  let newNode : Node := { ref := ref, inputs := inputs, outputs := (#[] : Array NodeRef), data := NodeData.nullData }
  -- Update outputs of all input nodes
  let updatedInputs := inputs.map (fun n => { n with outputs := n.outputs.push newNode })
  let newNode: Node := { newNode with inputs := updatedInputs }

  addNode2 newNode
  return newNode


def getIn(n: Node) (i: Fin n.inputs.size) : NodeRef :=
  n.inputs[i]

def nIns(n: Node): Nat :=
 n.inputs.size

def nOut(n: Node): Nat :=
  n.outputs.size

def isUnused(n: Node) : Bool :=
  n.outputs.size == 0

def isCFG: Bool :=
  false

end node
