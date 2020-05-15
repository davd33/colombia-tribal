(in-package #:dao)

;; TABLE DEFINITIONS
(eval-when
    (:compile-toplevel
     :load-toplevel)

  (mito:deftable user ()
    ((pname :col-type (:varchar 64))
     (password :col-type (:varchar 64))
     (mail :col-type (or (:varchar 64) :null)))
    (:unique-keys pname))

  (mito:deftable login ()
    ((token :col-type (:varchar 255))
     (expires :col-type :timestamp)
     (user :col-type user :references user))
    (:unique-keys user))

  )

(defparameter all-tables '(user login))

;; INSERT FROM JSON
(defun insert-user (json-user)
  "Insert user from json object."
  (mito:insert-dao (json->dao (defmapper user) json-user)))

(defun insert-login (json-login)
  "Insert login from json object."
  (mito:insert-dao (json->dao (defmapper login) json-login)))

;;; RETRIEVE CV
(defun retrieve-user (user-id)
  (mito:find-dao 'user :id user-id))

(defun retrieve-login-by-user-id (user-id)
  (first
   (mito:select-dao 'login
     (sxql:where (:= :user-id user-id))
     (sxql:limit 1))))
