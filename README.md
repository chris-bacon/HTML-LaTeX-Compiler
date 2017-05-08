# HTML-LaTeX Compiler

**Work in progress. This is my first compiler and first multi-file Haskell project.**

This is a source-to-source compiler that takes HTML and compiles it to LaTeX. The frontend and backend are written in Haskell.

## How to run

Linux: `./Main <filename>`
Windows: `Main.exe <filename>`

### What you should see:

> $ ./Main testOne.html
> Compiling testOne.html...
> Syntax: well-formed

## How it works

A compiler is typically split into a frontend and a backend. The frontend parses the source file into lexemes, which are then submitted to syntactic and semantic analysis. Syntactic and semantic analyses are handled by the syntactic and semantic analysers, or, in this case, the parser. This process creates a intermediate representation of the source, which is processed by the backend creating the target translation. (Optimisations are handled in the backend also, though this compiler does not make any optimisations.)

HTML is (at least) a context free language due to the fact that opening and closing tags have dependencies between them (and so some form of memory is required). Therefore, any grammar describing HTML needs to capture these dependencies, and dependencies are not capturable by a regular grammar, meaning that HTML needs at least a CFG to describe it. The parser handles these dependencies, assessing whether the source is syntactically well-formed HTML.

## TODO
### HTML Tags

- <div>
- <span>
- <strong>
- <sub>
- <sup>
- <meta>
- <title>
- <img>
- <a>
- quotations

## Bibliography

- Aho, A _et al_,. 2007, _Compilers: principles, techniques, and tool_ (second ed). Pearson.
- Sipser, M., 2005, _Introduction to the theory of computation_ (third ed), Cengage.
