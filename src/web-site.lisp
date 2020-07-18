(in-package #:web-site)

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(define-condition redirect (http-condition)
  ((location :initarg :location :accessor location)))

(defun redirect (location-path)
  (signal 'redirect :status-code 303 :location location-path))

;;; LOGIN WEB FORM
(defresource login (verb ct &key err) (:genpath login-path))

(defroute login
  (:get "text/html" &key err)
  (build-spinneret-html-response
    (html:login-form "/login-post"
                     err)))

(defroute login-post
  (:post "application/x-www-form-urlencoded")
  (let* ((payload (quri:uri-query-params (quri:make-uri :query (payload-as-string))))
         (pname (cdr (assoc "pname" payload :test #'string=)))
         (passwd (cdr (assoc "password" payload :test #'string=))))
    (if (null (dao:verify-login pname passwd))
        (redirect (str:concat "/login?err=" (hunchentoot:url-encode "Wrong id or password!")))
        (build-spinneret-html-response
          (html:logged-in-index pname)))))

;;; REGISTER
(defresource register (verb ct &key err) (:genpath register-path))

(defroute register
  (:get "text/html" &key err)
  (build-spinneret-html-response
    (html:register-form "Create your account"
                        "/register-post"
                        err)))

(defroute register-post
  (:post "application/x-www-form-urlencoded")
  (let ((payload (quri:uri-query-params (quri:make-uri :query  (payload-as-string)))))
    (labels ((param (name)
               (cdr (assoc name payload :test #'string=))))
      (let ((user-dto (handler-case
                          (progn
                            (assert (string= (param "password_repeat")
                                             (param "password")))
                            (make-instance 'dto:user
                                           :pname (param "pname")
                                           :password (param "password")
                                           :mail (param "mail")))
                        (simple-error ()
                          (redirect (str:concat "/register?err=" (hunchentoot:url-encode "Passwords are not the same.")))))))
        (services:register-user user-dto)
        (redirect (login-path :err "Registered!"))))))

(defmethod explain-condition ((c redirect) rs ct)
  (declare (ignore rs ct))
  (setf (hunchentoot:header-out :location) (location c))
  (format nil "See here: ~a" (location c)))

;;; HOME OF WEBSITE
(defroute home
  (:get "text/html")
  (build-spinneret-html-response
    (html:home "Interactive books")))

;;; COLOMBIA TRIBAL GAME
(defroute colombia-tribal
  (:get "text/html")
  (build-spinneret-html-response
    (let ((intro-story
           (hm:get colombia-tribal-game:|*stories*| "intro-story")))
      (html:story->html intro-story "Colombia Tribal" "the-mount.jpg"))))

(defroute story
  (:get "text/html" story-id)
  (build-spinneret-html-response
    (let ((story (hm:get colombia-tribal-game:|*stories*| (str:downcase story-id))))
      (html:story->html story
                        (interactive-text-book:story-title story)
                        (interactive-text-book:story-image story)))))

(defroute action
  (:get "text/html" action-id story-id)
  (build-spinneret-html-response
    (let ((action (hm:get colombia-tribal-game:|*actions*| (str:downcase action-id))))
      (html:action->html action
                         (interactive-text-book:id->title (str:downcase action-id))
                         (str:downcase story-id)
                         (interactive-text-book:action-image action)))))
