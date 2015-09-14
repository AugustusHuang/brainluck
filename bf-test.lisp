
(in-package :brainluck-test)

(defun bf-hello-world (&optional (stream *standard-input*))
  (top-level stream "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."))

(defun bf-echo (&optional (stream *standard-input*))
  (top-level stream ">+[[>],.----- ----- ---[+++++ +++++ +++[<]]>]<<[<]>>[.>]"))

(defun bf-reverse (&optional (stream *standard-input*))
  (top-level stream ",[>,]<[.<]"))
