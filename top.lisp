
(in-package :brainluck)

;;; Top level REPL.
(defun top-level ()
  "The repl of Brainluck, Brainfuck interpreter in Common Lisp."
  (let (;;; Global array, workes as the global brainfuck infinite array,
	;;; will double its size everytime to meet the program's need.
	(*global-array* (make-array 1024 :element-type 'unsigned-byte
				    :initial-element 0
				    :adjustable t))
	;;; Global pointer, workes as the global brainfuck pointer,
	;;; used as the index of the *GLOBAL-ARRAY*.
	(*global-pointer* 0))
    (loop
       (format t "~&TOP> ")
       (let ((line (string (read-line))))
	 (if (string= line "exit")
	     (return)
	     (funcall #'evaluate line))))))

;;; Internal interpreter function. Every time eats an input char, and do
;;; corresponding actions.
(defun evaluate (str)
  (let ((len (length str)))
    (loop for i from 0 to (1- len)
       do (case (char str i)
	    (#\>
	     (let ((len (length *global-array*)))
	       (if (/= *global-pointer* len)
		   (incf *global-pointer*)
		   (setf *global-array*
			 (adjust-array *global-array* (ash len 1)
				       :initial-element 0)
			 *global-pointer* (1+ *global-pointer*)))))
	    (#\<
	     (if (zerop *global-pointer*)
		 (error "Invalid operation.")
		 (decf *global-pointer*)))
	    (#\+
	     ;; Handle overflow by ourselves.
	     (if (= (aref *global-array* *global-pointer*) 255)
		 (error "Overflow.")
		 (incf (aref *global-array* *global-pointer*))))
	    (#\-
	     (if (= (aref *global-array* *global-pointer*) 0)
		 (error "Underflow.")
		 (decf (aref *global-array* *global-pointer*))))
	    (#\.
	     (format t "~C~%"
		     (code-char (aref *global-array* *global-pointer*))))
	    (#\,
	     (setf (aref *global-array* *global-pointer*) (read-char)))
	    (#\[
	     (when (zerop (aref *global-array* *global-pointer*))
	       ;; Expect the enclosing ].
	       (unless (char= (char str i) #\])
		 (incf i))))
	    (#\]
	     (when (not (zerop (aref *global-array* *global-pointer*)))
	       ;; Expect the nearest starting [.
	       (unless (char= (char str i) #\[)
		 (decf i))))))))
