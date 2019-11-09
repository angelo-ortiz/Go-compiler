TODO
----
1. Lexer
  * Check *next_token* function
1. Parser
  * Check syntax error messages
  * How to get the pos° of the first '-' when dealing with integers (for error printing purposes)?
1. Semantic analyser
1. Assembly-code producer

Remarks
-------
1. ~~Uncapable of parsing -2^63~~
1. Files can only belong to package __main__
1. Cannot import package apart from __fmt__
1. Functions must not be struct fields (thankfully, `type` ensures it)


Fixed issues
------------
1. ~~Before-parenthesis comma~~ No longer an issue, since dealing manually in parser
1. ~~Before-brace semicolon~~ Idem
1. Dangling semicolons in block
1. Conflict between setting expressions (=) and sugar syntax for initialising variables (:=)
1. Parsing of all integers in [-2^63;2^63-1] is now successful
1. Precise location for errors concerning the LHS of :=
