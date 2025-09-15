import LeanSon.Lexer
import LeanSon.Node
open node
open lexer


structure Parser where
  lexerL: Lexer
  startN: M Node

namespace Parser
def ParserMk (source:String ) : Parser :=
{lexerL:= LexerMK source, startN:= NodeMK #[]}


end Parser
