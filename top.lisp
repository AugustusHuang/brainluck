
(in-package :brainluck)

;;; Top level REPL.
(let (;;; Global array, workes as the global brainfuck infinite array,
      ;;; will double its size everytime to meet the program's need.
      (global-array (make-array 1024 :element-type 'unsigned-byte
				:initial-element 0 :adjustable t))
      ;;; Global pointer, workes as the global brainfuck pointer,
      ;;; used as the index of the *GLOBAL-ARRAY*.
      (global-pointer 0))
  (defun top-level ()
    "The repl of Brainluck, Brainfuck interpreter in Common Lisp."
    (loop
       (format t "~&TOP> ")
       (let ((line (string (read-line))))
	 (cond ((string= line "clear")
		(clear-array))
	       ((string= line "exit")
		(progn (clear-array) (return)))
	       (t
		(funcall #'evaluate line))))))

  (defun clear-array ()
    (progn
      (loop for i from 0 to (1- (length global-array))
	 do (setf (aref global-array i) 0))
      (setf global-pointer 0)))
;;; Internal interpreter function. Every time eats an input char, and do
;;; corresponding actions.
  (defun evaluate (str)
    (let ((i 0))
      (loop for len = (length str)
	 while (/= i len)
	 do (case (char str i)
	      (#\>
	       (let ((len (length global-array)))
		 (if (/= global-pointer len)
		     (progn
		       (incf global-pointer)
		       (incf i))
		     (setf global-array
			   (adjust-array global-array (ash len 1)
					 :initial-element 0)
			   global-pointer (1+ global-pointer)
			   i (1+ i)))))
	      (#\<
	       (if (zerop global-pointer)
		   (error "Invalid operation.")
		   (progn
		     (decf global-pointer)
		     (incf i))))
	      (#\+
	       ;; Handle overflow by ourselves.
	       (if (= (aref global-array global-pointer) 255)
		   (error "Overflow.")
		   (progn
		     (incf (aref global-array global-pointer))
		     (incf i))))
	      (#\-
	       (if (zerop (aref global-array global-pointer))
		   (error "Underflow.")
		   (progn
		     (decf (aref global-array global-pointer))
		     (incf i))))
	      (#\.
	       (format t "~C~%"
		       (code-char (aref global-array global-pointer)))
	       (incf i))
	      (#\,
	       (setf (aref global-array global-pointer) (read-char)
		     i (1+ i)))
	      (#\[
	       (when (zerop (aref global-array global-pointer))
		 ;; Expect the nearest ending ].
		 (loop while (not (char= (char str i) #\]))
		      do (incf i)))
	       (incf i))
	      (#\]
	       (when (not (zerop (aref global-array global-pointer)))
		 ;; Expect the nearest starting [.
		 (loop while (not (char= (char str i) #\[))
		      do (decf i)))
	       (incf i))
	      (t
	       (incf i)))))))
