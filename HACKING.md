The phases of compilation:

* (Main) Create initial AST consisting of include nodes for files passed on
  the commandline.
* (File) Transform file nodes:
  * (File) Load a file into a string.
  * (Lexer) Lex file into Lexemes.
  * (Parser) Parse file to SourceAST nodes.
  * (File) Insert new AST nodes into existing AST, recursively call self.
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
* (Main) Output the final bytestring, or report errors.

Any AST nodes which cause a problem in one of these passes are replaced by Err
nodes, which are passed through the remaining pipeline unchanged. This allows
us to give the user error messages corresponding to the earliest point in the
pipeline a problem was identified, and also to collect as many errors as
possible before reporting to the user, instead of simply bailing after the
first error is found.

Macros can refer to other macros. Note that if there is a cyclical definition,
the compiler will try to loop forever during expansion and eventually crash.

Files can also include eachother in a nested fashion. Again, if there is a
cycle, the compiler will try to loop forever and eventually crash.

TODO list:

* Include statements. Probably handle these in the parser?
* Collect symbol/bank information and dump a symbol file alongside the .bin.
* Generate names for .bin files better.
* Conditional compilation.
* Give unique names to duplicate opcodes. Check that my opcodes are all
  correct, esp. stuff like "and a" which I have as "and a, a".
* Cobble together some translation rules for rgbas syntax.
* Write tests.
