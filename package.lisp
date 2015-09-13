(defpackage :brainluck
  (:use :cl)
  (:documentation
   "Main package of Brainluck, an implementation of Brainfuck in Common Lisp.")
  (:export :top-level))

(defpackage :brainluck-test
  (:use :cl
	:brainluck)
  (:documentation
   "Test package of Brainluck.")
  (:export :hello-world
	   :echo))
