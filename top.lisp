
(in-package :brainluck)

;;; Top level REPL.
(let (;;; Global array, workes as the global brainfuck infinite array,
      ;;; will double its size everytime to meet the program's need.
      ;;; By default the size is 30K, like the common definition.
      (global-array (make-array 32768 :element-type 'unsigned-byte
				:initial-element 0 :adjustable t))
      ;;; Global pointer, workes as the global brainfuck pointer,
      ;;; used as the index of the *GLOBAL-ARRAY*.
      (global-pointer 0))
  (defun top-level (&optional (stream *standard-input*) form)
    "The repl of Brainluck, Brainfuck interpreter in Common Lisp."
    (if form
	(evaluate stream form)
	(loop
	   (format t "~&TOP> ")
	   (let ((line (string (read-line))))
	     (cond ((string= line "clear")
		    (clear-array))
		   ((string= line "exit")
		    (progn (clear-array) (return)))
		   (t
		    (evaluate stream line)))))))

  (defun clear-array ()
    (progn
      (loop for i from 0 to (1- (length global-array))
	 do (setf (aref global-array i) 0))
      (setf global-pointer 0)))
;;; Internal interpreter function. Every time eats an input char, and do
;;; corresponding actions.
  (defun evaluate (stream str)
    (let ((i 0)
	  ([-list nil))
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
		   (progn
		     (warn "Reach the leftmost slot of the global array.")
		     (return))
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
	       (unless (zerop (aref global-array global-pointer))
		 (decf (aref global-array global-pointer)))
	       (incf i))
	      (#\.
	       (format t "~C" (code-char (aref global-array global-pointer)))
	       (incf i))
	      (#\,
	       (setf (aref global-array global-pointer)
		     (let ((chara (read-char stream nil :eof)))
		       (if (or (eql chara :eof) (char= chara #\Newline))
			   0
			   (char-code chara)))
		     i (1+ i)))
	      (#\[
	       (push i [-list)
	       (when (zerop (aref global-array global-pointer))
		 ;; Expect the corresponding ending ].
		 (loop while (not (char= (char str i) #\]))
		      do (incf i)))
	       (incf i))
	      (#\]
	       (let (([-pos (pop [-list)))
		 (when (not (zerop (aref global-array global-pointer)))
		   ;; Expect the nearest starting [.
		   (setf i [-pos)
		   (push i [-list))
		 (incf i)))
	      (t
	       (incf i)))))))
