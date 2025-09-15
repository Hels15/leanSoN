-- Defines SON lexer

structure Lexer where
 source: String
 position : String.Pos
deriving Repr

namespace lexer

def LexerMK( sourcel: String) : Lexer :=
  {source:=sourcel, position:=0}

def isEOF(lexer: Lexer): Bool :=
 lexer.position >= lexer.source.endPos


def peekChar (lexer : Lexer) : Char :=
  if isEOF lexer then '\x00'   -- null character
  else lexer.source.get! lexer.position

def nextChar (lexer : Lexer) : Lexer × Char :=
  let c := peekChar lexer
  let newPos := lexer.source.next lexer.position
  ({ lexer with position := newPos }, c)


end lexer
