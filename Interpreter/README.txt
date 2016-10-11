This is an interpreter for a self-created language. It was build during the Programming Languages and Paradigms course. It was implemented using Haskell programming language.

To use interpreter:
1. make
2. ghc --make interpreter.hs
3. ./interpreter <program_path>

The program should define a no-argument function “main”, which is invoked when running a program. For a sample code, see a good/bad directory. For the language grammar, check "mich.cf" file.

The language supports:
- functions with parameters passed by variable
- recursion
- two pre-defined types (int, bool)
- pre-defined “print” procedure
- a “for” loop
- C-type operators (++, +=, etc.)
- variable shadowing with static binding
- functions as parameters to functions
- anonymous functions
