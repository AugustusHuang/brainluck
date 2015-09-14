# brainluck
Brainfuck interpreter in Common Lisp.

It is annoying to leave emacs when I want to play with some BF when
working with Common Lisp,
so I implement this interpreter.

## Usage
In SBCL, use internal REQUIRE method, `(require :brainluck)` and
`(require :brainluck.test)`, which will load the top-level and test functions.

## NOTE
There's still something wrong when handling EOF and Newline, be careful if you
want to run some codes that will eat them.
