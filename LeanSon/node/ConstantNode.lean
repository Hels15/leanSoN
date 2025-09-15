import LeanSon.Node
namespace ConstantNode

open node

def setControl (n : Node) (ctrl : Node) : Node :=
  let newInputs :=
    if n.inputs.size > 0 then
      n.inputs.set! 0 ctrl
    else
      #[ctrl]
  { n with inputs := newInputs }


def NodeMk (nodel : Node) (control : Node) (value : Int) : node.Node :=
  --let newNode: Node := { nodel with value := value }
  --let newNode := setControl newNode control
  --newNode
  control 

end ConstantNode
