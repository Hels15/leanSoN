import LeanSon
namespace Parser

structure Parser where
  lexerL: lexer
  startN: node

def ParserMk (source:String ) : Parser :=
{lexerL:= Lexer.empty, startN:= Node.empty}


end Parser
