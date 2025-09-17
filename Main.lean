import LeanSon


def test (a : Int) : Int := a + 1


def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

-- Array removing

structure Node where
  nid     : Nat
  inputs  : Array Node := #[]
  outputs : Array Node := #[]
deriving Repr

-- Remove nodes with a specific id from an array
def removeNodeById (arr : Array Node) (idToRemove : Nat) : Array Node :=
  arr.filter (fun n => n.nid ≠ idToRemove)

-- Example
def n1: Node := { nid := 1 }
def n2: Node := { nid := 2 }
def n3: Node := { nid := 3 }

#eval removeNodeById #[n1, n2, n3] 2
-- Output: #[{nid := 1, inputs := #[], outputs := #[]}, {nid := 3, inputs := #[], outputs := #[]}]

structure Test where
  a : Nat
  b : String
deriving Repr

structure Test2 extends Test where
  c : Bool
deriving Repr

-- constructing a Test2
def ex : Test2 :=
  { toTest := { a := 10, b := "hello" }, c := true }

-- DecidableEQ already uses BEq
structure Cat where
 name: String
 age: Nat

deriving Repr,  DecidableEq

structure CatOrDog extends Cat where
   dog: Bool
deriving Repr,  DecidableEq

structure Dog2 extends Cat where
   dog: Bool
deriving Repr, DecidableEq

def cat1 :CatOrDog := {toCat := { name := "Whiskers", age := 3 }, dog := false }
def dog1 :CatOrDog := {toCat := { name := "Whiskers", age := 3 }, dog := true }

-- Can do this becuase DecidableEQ imports BEq
#eval cat1 == dog1  -- false
#eval cat1 == cat1  -- true

def example2 : cat1 = cat1 := by
  rfl

-- Example

structure Child where
  name: String
  age: Nat
  deriving Repr

def Child1: Child := { name:= "Alice", age := 5 }

-- doesnt change
def changeName(c: Child) (name: String): Child :=
  {c with name:= name}


#eval Child1
#eval changeName Child1 "Bob"
#eval Child1

abbrev N: Type := Nat
def thirtyNine: N:= 39
#check thirtyNine

-- Lean has great pattern matching which makes inductive much better than whatever you'd do to mimic inheritance

-- Arena that modifies them

-- Inhabited provides default data
-- Inhabited on a structure (defaults everything that a structure has what happens if struscture doesnt have default)


structure NodeRef where
  nid: Nat
deriving Inhabited, Repr


inductive NodeData where
   | constantl (value: Int64)
   | nullData
deriving Inhabited, Repr

structure Node1 where
  nid     : Nat
  inputs  : Array NodeRef := #[]
  outputs : Array NodeRef := #[]
  data  :   NodeData-- optional NodeData
deriving Inhabited, Repr

structure ManyNodes where
    uniqueNodeId : Nat
    allNodes : Array Node1

abbrev M := StateRefT ManyNodes IO

def updateNode (idx : Nat) (f : Node1 → Node1) : M Unit := do
  modify fun s =>
    { s with allNodes := s.allNodes.modify idx f }

-- Need to look up the ref based on the nid
def updateInputsOutputsNid (inputs : Array NodeRef) (newNode : Node1) : M Unit := do
  for ref in inputs do
    updateNode ref.nid (fun n => { n with outputs := n.outputs.push newNode.nid })

-- Add a custom node to the arena
def addNode2 (newNode : Node1) : M Unit := do
  -- update the arena: increment unique ID and store the new node
  modify fun arena =>
    { arena with
      uniqueNodeId := arena.uniqueNodeId + 1,
      allNodes     := arena.allNodes.push newNode }

  def NodeMK (inputs : Array NodeRef := #[]) : M Node1 := do
  let uid := (← get).uniqueNodeId
  let newNode : Node1 := { nid := uid, inputs := inputs, outputs := (#[] : Array NodeRef), data := NodeData.nullData }
  -- Update outputs of all input nodes
  let updatedInputs := inputs.map (fun n => { n with outputs := n.outputs.push newNode })
  let newNode: Node1 := { newNode with inputs := updatedInputs }

  addNode2 newNode
  return newNode
-- Node structure
                        -- either no data or not a
def nodeConstant : Node1 := {nid:= 0, data := NodeData.constantl 42 }
def nodeAdd : Node1      :=  {nid:= 0,      data  := NodeData.nullData}
#eval nodeConstant

def example1 : Int64 :=
  if let .constantl val := nodeAdd.data then
    val
  else
    panic! "Not a constant node"


-- Dot syntax

structure ABC where

  x: Int
  y: Int

def point: ABC := { x := 10, y := 20 }

def ABC.ab(a: ABC) : Int :=
  a.x + a.y

#eval point.ab
#eval example1
