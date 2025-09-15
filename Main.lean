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
