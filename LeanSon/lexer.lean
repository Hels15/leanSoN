

namespace Lexer
structure lexer where
 source: String
 position : String.Pos
deriving Repr

def LexerMk( sourcel: String) : lexer :=
  {source:=sourcel, position:=0}

def isEOF(lexer: lexer) :=
 lexer.position >= lexer.source.endPos


def peekChar (lexer : Lexer.lexer) : Char :=
  if isEOF lexer then '\x00'   -- null character
  else lexer.source.get! lexer.position

def nextChar (lexer : lexer) : Char :=
  let c := peekChar lexer
  let newPos := lexer.source.next lexer.position
  { lexer with position := newPos }
  c


end Lexer
