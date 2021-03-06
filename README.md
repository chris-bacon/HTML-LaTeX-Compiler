# Htex
## HTML-LaTeX Compiler

**Proof of Concept/Learning exercises. This is my first compiler and first multi-file Haskell project.**

This is a (limited) source-to-source compiler that takes HTML and compiles it to LaTeX. The frontend and backend are written in Haskell.

## How to build and run

```
stack build
stack exec -- htex <filename.html>
```

### What you should see:

```
> Compiling filename.html...
> Syntax: well-formed
> LaTeX successfully compiled
```

Alternatively, if there is a syntax error, then the compiler will throw it out.

### Tests

You can run the tests with:

```
stack test
```

## How it works

A compiler is typically split into a frontend and a backend. The frontend parses the source file into lexemes, which are then submitted to syntactic and semantic analysis. Syntactic and semantic analyses are handled by the syntactic and semantic analysers, or, in this case, the parser. This process creates a intermediate representation of the source, which is processed by the backend creating the target translation. (Optimisations are handled in the backend also, though this compiler does not make any optimisations.)

HTML is (at least) a context free language due to the fact that opening and closing tags have dependencies between them (and so some form of _unbounded_ memory is required). Therefore, any grammar describing HTML needs to capture these dependencies, and dependencies are not capturable by a regular grammar, meaning that HTML needs at least a context-free grammar to describe it. The parser handles these dependencies, assessing whether the source is syntactically well-formed HTML.

### Example

Suppose we have the following HTML:

`<p><b><i>red dog</i></b></p>`

This will be passed into the Lexical Analyser, which will parse the input into a series of lexical tokens and their attributes:

`[("sTag", "p"), ("sTag", "bold"), ("sTag", "italics"), ("Word", "red"), ("Word", "dog"), ("eTag", "italics"), ("eTag", "bold"), ("eTag", "p")]`

This will then undergo syntactic analysis, which will determine whether it is of a valid form or not. If it is not valid, an error will be thrown. Otherwise, it will be passed on to the code generator, which will produce the following LaTeX code:

`\textbf{\textit{red dog}}`

## TODO

- \<div\>
- \<span\>
- \<strong\>
- \<sub\>
- \<sup\>
- \<meta\>
- \<title\>
- \<img\>
- \<a\>
- quotations
- Add spaces and newlines back into code generator
- Comments

## Bibliography

- Aho, A _et al_,. 2007, _Compilers: principles, techniques, and tools_ (second ed). Pearson.
- Sipser, M., 2005, _Introduction to the theory of computation_ (third ed), Cengage.
