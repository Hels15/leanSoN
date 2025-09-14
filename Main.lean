import LeanSon


def test (a : Int) : Int := a + 1


inductive MyBool
| true
| false

def flip1 : MyBool → MyBool
| MyBool.true  => MyBool.false
| MyBool.false => MyBool.true

structure Test where
 a: Nat
 b: Nat



def test1: Test := {a:=1, b:=2}
def change(x: Nat) (myTest: Test) : Test :=
{myTest with a:=x}


def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

#eval flip1 MyBool.false
#eval change 4 test1
#check test1
