

--A private Global Static mutable counter, for unique node id generation.
-- To make the compiler multi-threaded, this field will have to move into a TLS.
-- Starting with value 1, to avoid bugs confusing node ID 0 with uninitialized values.


structure Node where

nid:     Nat
inputs:  Array  Node := #[]
outputs: Array  Node := #[]


-- Node arena
structure ManyNodes where
    uniqueNodeId : Nat
    allNodes : Array Node

abbrev M := StateRefT ManyNodes IO

-- Each node has a unique dense Node ID within a compilation context
-- The ID is useful for debugging, for using as an offset in a bitvector,
-- as well as for computing equality of nodes (to be implemented later).
--

namespace node
--        ///////// ARENA    ///////////
-- Add empty node to the Arena
def addNode1 : M Node := do
  let newNode := { nid := (← get).uniqueNodeId }
  modify fun arena =>
  { arena with
    uniqueNodeId := arena.uniqueNodeId + 1,
    allNodes     := arena.allNodes.push newNode }
  return newNode

-- Add a custom node to the arena
def addNode2 (newNode : Node) : M Unit := do
  -- update the arena: increment unique ID and store the new node
  modify fun arena =>
    { arena with
      uniqueNodeId := arena.uniqueNodeId + 1,
      allNodes     := arena.allNodes.push newNode }


--        ///////// ARENA    ///////////

-- Create a new node and update outputs of input nodes
def NodeMK (inputs : Array Node := #[]) : M Node := do
  let uid := (← get).uniqueNodeId
  let newNode : Node := { nid := uid, inputs := inputs, outputs := (#[] : Array Node) }
  -- Update outputs of all input nodes
  let updatedInputs := inputs.map (fun n => { n with outputs := n.outputs.push newNode })
  let newNode: Node := { newNode with inputs := updatedInputs }

  addNode2 newNode
  return newNode


def getIn(n: Node) (i: Fin n.inputs.size) : Node :=
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
