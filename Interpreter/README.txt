This is an interpreter for a self-created language. It was build during the Programming Languages and Paradigms course. It was implemented using Haskell programming language. The parser was generated using BNF Converter.

To use interpreter:
1. make
2. ghc --make interpreter.hs
3. ./interpreter <program_path>

The program should define a no-argument function “main”, which is invoked when running a program. For a sample code, see a good/bad directory. For the language grammar in BNF notation, check "mich.cf" file.

The language supports:
- functions with parameters passed by variable
- recursion
- variable shadowing with static binding
- two pre-defined types (int, bool)
- a pre-defined “print” procedure
- C-type operators (++, +=, etc.)
- functions as parameters to functions
- anonymous functions
