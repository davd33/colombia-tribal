(in-package #:dto)

(defun mapdto ()
  "Like a map function for dto instances.")

(defclass user ()
  ((pname :initarg :pname
          :accessor pname
          :type string
          :documentation "Username.")
   (password :initarg :password
             :accessor password
             :type string
             :documentation "Password of the user.")
   (mail :initarg :mail
         :accessor mail
         :type string
         :documentation "Mail address.")))

(defclass login ()
  ((token :initarg :token
          :accessor token
          :type string
          :documentation "Token identifying the user's session.")
   (expires :initarg :expires
            :accessor expires
            :type string
            :documentation "When the token will be invalid.")
   (user :initarg :user
         :accessor user
         :type user-dto
         :documentation "User DTO.")))
