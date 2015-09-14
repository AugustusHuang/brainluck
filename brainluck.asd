
(in-package :asdf-user)

(defsystem :brainluck
    :name "brainluck"
    :description "Brainluck, a Brainfuck interpreter in Common Lisp."
    :version "0.0.1"
    :author "Augustus Huang <augustushwang@gmail.com>"
    :components ((:file "package")
		 (:file "top" :depends-on ("package"))))

(defsystem :brainluck.test
    :depends-on (:brainluck)
    :name "brainluck-test"
    :description "Test suite of brainluck."
    :version "0.0.1"
    :author "Augustus Huang <augustushwang@gmail.com>"
    :components ((:file "bf-test")))
