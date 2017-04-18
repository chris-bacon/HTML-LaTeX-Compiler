# HTML-LaTeX Compiler

**Work in progress**

**This is my first attempt at writing a compiler, so is most definitely very bad and rubbish.**

This is a source-to-source compiler that takes HTML as the source language and compiles it into LaTeX. At the moment this compiler only works for a subset of HTML. The HTML must also be well-formed. That is, it must not contain malformed tags, as the grammar does not target such tags. For example, *< b>* will not be compiled as the LaTeX equivalent of the bold tag but will instead be compiled as the literal string "< b>". It would be possible to write a grammar that compiles *at least some* malformed HTML, but for now this compiler deals solely with valid HTML.

## How to run

The program is written in Haskell, which is compiled to an executable. To run, simply run this executable.

Linux: `./Main`
Windows: `Main.exe`

## TODO
### HTML Tags

- <div>
- <span>
- <strong>
- <sub>
- <sup>
- <meta>
- <title>
- quotations
- mathematical equations/expressions
