(in-package #:dao.utils)

(defparameter *connection* nil)

(defun connect (&key (db-name "colombia_tribal") (username "postgres") (password "helloworld"))
  (when (null *connection*)
    (setf *connection*
          (mito:connect-toplevel :postgres
                                 :database-name db-name
                                 :username username
                                 :password password))))

;; MAPPERS
(defmacro make-mapper (kind mappings)
  "Return a function of a mapper."
  `(let ((map (make-hash-table)))
     ,@(reduce #'(lambda (acc curr) (append acc `((hm:put map ,@curr))))
               mappings
               :initial-value `())
     (make-json->dao-mapper :hm map
                            :kind ,kind)))

(defstruct json->dao-mapper
  hm
  kind)

(defmacro defmapper (table)
  "Creates a default mapper for TABLE."
  (let ((table-fields
         (remove-if-not #'(lambda (elt)
                            (eq (find-package :dao) (symbol-package elt)))
                        (mapcar #'(lambda (elt)
                                    (closer-mop:slot-definition-name elt))
                                (mop:class-slots table)))))
    `(make-mapper ',table
         ,(loop for f in table-fields
             collect `(',f ,(intern (str:upcase f) :keyword))))))

;; MAP A JSON TO A DAO
(defun json->dao (mapper json)
  "Fills the dao from the given JSON object and according to the mapper."
  (let* ((m-kind (json->dao-mapper-kind mapper))
         (dao (make-instance m-kind)))
    (maphash #'(lambda (k v)
                 (setf (slot-value dao k) (jsons:get-in json v)))
             (json->dao-mapper-hm mapper))
    dao))

;; CREATE/DROP TABLES
(defun create-table (table-type)
  "Creates the table of given type."
  (restart-case
      (when (not (mito.db:table-exists-p *connection*
                                         (mito.class:table-name (find-class table-type))))
        (format t "~&CREATE TABLE: ~A" table-type)
        (mapc #'mito:execute-sql (mito:table-definition table-type))
        (mito:ensure-table-exists table-type)
        t)
    (skip () nil)))

(defun drop-table (table-type)
  "Drops table of given type."
  (restart-case
      (when (mito.db:table-exists-p *connection*
                                    (mito.class:table-name (find-class table-type)))
        (format t "~&DROP TABLE: ~A" table-type)
        (mito:execute-sql (sxql:drop-table table-type))
        t)
    (skip () nil)))

(defun create-tables (tables)
  (mapcar #'create-table tables))

(defun drop-tables (tables)
  (mapcar #'drop-table tables))

(defun reset-db-tables (tables)
  (drop-tables tables)
  (create-tables tables))
