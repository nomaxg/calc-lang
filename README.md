Simple interpreted calculator language written in Haskell, with [Parsec](https://hackage.haskell.org/package/parsec) as the parser. 

Inspiration/some code taken from: 
- [Exploring Languages with Interpreters
and Functional Programming](https://john.cs.olemiss.edu/~hcc/csci450/ELIFP/Ch42/42_Abstract_Syntax.html)
- [Implementing a JIT Compiled Language with Haskell and LLVM](https://www.stephendiehl.com/llvm/)

## Usage
```console
$ ./main
ready> 2.0;
2.0
ready> 4.123 % 1.254;
0.3610000000000002
ready> a = 5.2;
5.2
ready> b = a + (c = 2.25);
7.45
```
