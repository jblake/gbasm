The phases of compilation:

* (Main) Load file into a String.
* (Lexer) Lex file into Lexemes.
* (Parser) Parse file to SourceAST nodes.
* (Macros) Replace macro references with their definitions.
* (UnresolvedMacros) Replace unresolved macro references with errors.
* (Math) Perform early simplification of operands. We have to do this now
  because we're about to process OutPos nodes.
* (OutputPos) Decorate with addresses, changing to FullAST representation.
* (Relative) Rewrite operands that use relative addresses to use global
  addresses and subtraction operations instead.
* (Globals) Replace global references with absolute addresses.
* (Locals) Replace local references with absolute addresses.
* (Unresolved) Replace unresolved references with errors.
* (Math) Perform late simplification of operands. We weren't able to do this
  completely up until now because of unresolved references.
* (CompileOps) Transform opcodes into bytestrings.
* (IncBin) Load include statements into memory as bytestrings.
* (Main) Collect and report error nodes to the user.
* (ByteGen) Transform FullAST nodes into a single contiguous bytestring.
* (Main) Output the final bytestring.

Any AST nodes which cause a problem in one of these passes are replaced by Err
nodes, which are passed through the remaining pipeline unchanged. This allows
us to give the user error messages corresponding to the earliest point in the
pipeline a problem was identified, and also to collect as many errors as
possible before reporting to the user, instead of simply bailing after the
first error is found.
