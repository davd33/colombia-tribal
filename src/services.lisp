(in-package #:services)

(defmacro defpost (name (&rest args) (&rest json-args) doc &body body)
  "Creates a function that contains code to validate that every JSON-ARGS
given to a function call is checked against the desired class.

The JSON-ARGS arguments shall be lists of two elements:
 - the name of the variable
 - type to which the json should be compatible with.

Example:
\(defpost hello \(a b c\) \(\(j1 'class-type\) \(j2 'class-type\)\)
  ...docstring...
  ...body using a, b, c, j1, and j2...\)"
  `(defun ,name ,(append args (mapcar #'first json-args))
     ,(str:concat doc)
     ,@(reduce #'(lambda (acc j-a) (append acc `((assert (listp ,(first j-a)))
                                                 (assert (jsons:type-compatible-p ,@j-a)))))
               json-args
               :initial-value (list))
     ,@body))

(defun get-story (id)
  "Return the text for the next STORY."
  "Hello world!")
