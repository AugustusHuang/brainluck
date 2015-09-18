# brainluck
Brainfuck interpreter in Common Lisp.

It is annoying to leave emacs when I want to play with some BF when
working with Common Lisp,
so I implement this interpreter.

## Usage
Firstly clone this repo into your common-lisp/ or /.local/share/common-lisp/source
which are the standard path ASDF will search in. Of course in home directory.
In SBCL, use internal REQUIRE method, `(require :brainluck)` and
`(require :brainluck.test)`, which will load the top-level and test functions.
The test functions will run only once, and so the loop you create, I feel really
tired to get into the debug loop everytime I wanna quit the current loop.

## NOTE
EOF and Newline errors are fixed, now it only evaluate once and these two chars
will be regarded as 0, so the loop will terminate then.
