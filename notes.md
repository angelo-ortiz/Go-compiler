TODO
----
1. (OK) Lexer
1. (OK) Parser
1. Semantic analyser
  * Fix print args
1. Assembly-code producer
  * (OK) Sem -> IS translation
  * (OK) IS -> RTL translation
  * ERTL -> LTL translation
  * LTL -> LIN transaltion

Remarks
-------
1. ~~Uncapable of parsing -2^63~~
1. Files can only belong to package __main__
1. Cannot import package apart from __fmt__
1. Functions must not be struct fields (thankfully, `type` ensures it)
1. Finished -> IS -> RTL -> ERTL translations
1. Finished liveness & interference analyses, and register allocation (colouring)
1. Print should be implemented with assembly's primitive *printf*
1. Escaped character *%* in lexer for function print 
1. Must make a last check pass to delete futile *nop*s (Union-Find) when translationg to LTL

Fixed issues
------------
1. ~~Before-parenthesis comma~~ No longer an issue, since dealt with manually in parser
1. ~~Before-brace semicolon~~ Idem
1. ~~Dangling semicolons in block~~ Same error as above
1. Conflict between setting expressions (=) and sugar syntax for initialising variables (:=)
1. Parsing of all integers in [-2^63;2^63-1] is now successful
1. Precise location for errors concerning the LHS of :=
