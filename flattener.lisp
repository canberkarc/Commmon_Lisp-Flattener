; Setting LISP attribute as invert to make LISP case-sensitive because as default LISP converts all symbols to uppercase
; *readtable* is an object in LISP structure and "readtable-case" is its attribute
(setf (readtable-case *readtable*) :invert)

; Function to read nested list and flatten it
(defun flattener (name)
  ; Function to store lists into line
  (defun lists-into-line (line)
    (read-from-string (concatenate 'string "(" line ")")
    )
  )

  ; New empty list
  (setf lst (list nil)) 

  ; Push list to empty list
  (push (mapcar #'lists-into-line  (with-open-file (in name)
    (loop for line = (read-line in nil)
      while line collect line))) lst)
  
  ; Flatten nested list
  (let (res)
    (labels ((grep (lst)
               (cond ((null lst) nil)
                     ((atom lst) (push lst res))
                     (t (grep (rest lst))
                        (grep (first lst))))))
      (grep lst)
      res))
)

;Function to write flattened nested list into output file
(defun write-to-file()
  (with-open-file  
     (stream "flattened_list.txt" ; Output file
         :direction :output    
         :if-exists :overwrite ; Overwrite if exists
         :if-does-not-exist :create) ; Create if doesn't exist
     (format stream "~A~%" 
      (flattener "nested_list.txt") ; Nested list is given to flattener
    )
  )
)

(write-to-file)