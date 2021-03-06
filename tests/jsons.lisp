;;; DEFINE SOME DTO CLASSES
(defpackage colombia-tribal/tests/jsons-dtos
  (:use :cl)
  (:export :acar
           :color
           :wheel))
(in-package :colombia-tribal/tests/jsons-dtos)

(defclass color ()
  (r g b))

(defclass wheel ()
  ((radius :type integer)))

(defclass acar ()
  ((size :type integer)
   (color :type color)
   radio-o
   (wheel-list :type wheel)))

(defpackage colombia-tribal/tests/jsons
  (:use :cl
        :rove))
(in-package :colombia-tribal/tests/jsons)

(defun type-comp-acar? (json-string)
  "Decode JSON-STRING and checks type compatibility with 'ACAR class."
  (jsons:type-compatible-p
   (json:decode-json-from-string json-string) 'colombia-tribal/tests/jsons-dtos:acar))

(defun type-comp-acar[]? (json-string)
  "Decode JSON-STRING and checks type compatibility for a list of 'ACAR objects."
  (jsons:type-compatible-p
   (json:decode-json-from-string json-string)
   'colombia-tribal/tests/jsons-dtos:acar
   t))

(defun type-comp-wheel? (json-string)
  "Decode JSON-STRING and checks type compatibility with 'WHEEL class."
  (jsons:type-compatible-p
   (json:decode-json-from-string json-string) 'colombia-tribal/tests/jsons-dtos:wheel))

(defparameter acar-ok "{\"size\":12,\"radio-o\":\"y\",\"wheel-list\":[{\"radius\":10}],\"color\":{\"r\":12,\"g\":233,\"b\":2}}")

(defparameter awheel-ok "{\"radius\":12}")

(defparameter acar-array-ok "[{\"size\":12,\"wheel-list\":[{\"radius\":10}],\"color\":{\"r\":12,\"g\":233,\"b\":2}},{\"size\":12,\"wheel-list\":[{\"radius\":10}],\"color\":{\"r\":12,\"g\":233,\"b\":2}}]")

(deftest test-type-compatible-p
  (testing "all compatible"
    (ok (type-comp-acar? acar-ok)))
  (testing "depth-1 field doesn't match slot name"
    (ng (type-comp-acar? "{\"siz\":12,\"wheel-list\":[{\"radius\":10}]},\"color\":{\"r\":12,\"g\":233,\"b\":2}")))
  (testing "depth-2 field doesn't match slot name"
    (ng (type-comp-acar? "{\"size\":12,\"wheel-list\":[{\"radiu\":10}]},\"color\":{\"r\":12,\"g\":233,\"b\":2}")))
  (testing "depth-2 field should have been an array"
    (ng (type-comp-acar? "{\"size\":12,\"wheel-list\":{\"radius\":10},\"color\":{\"r\":12,\"g\":233,\"b\":2}}")))
  (testing "a JSON object could simply be an array of objects!"
    (ok (type-comp-acar[]? acar-array-ok)))
  (testing "this json should have been an array!"
    (ng (type-comp-acar[]? "{\"size\":12,\"wheel-list\":[{\"radius\":10}],\"color\":{\"r\":12,\"g\":233,\"b\":2}}")))
  (testing "it's ok to miss an optional field (e.g. field-o)"
    (ok (type-comp-acar? "{\"size\":12,\"wheel-list\":[{\"radius\":10}],\"color\":{\"r\":12,\"g\":233,\"b\":2}}"))))
