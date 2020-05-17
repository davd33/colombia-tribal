(in-package #:web-site)

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(define-condition redirect (http-condition)
  ((location :initarg :location :accessor location)))

;;; REGISTER WEB FORM
(defresource register (verb ct &key err) (:genpath register-path))

(defroute register
  (:get "text/html" &key err)
  (build-spinneret-html-response
    (html:register-form "Create your account"
                        "/register-post"
                        err)))

;;; REGISTER FORM POST
(defroute register-post
  (:post "application/x-www-form-urlencoded")
  (let ((payload (quri:uri-query-params (quri:make-uri :query  (payload-as-string)))))
    (labels ((redirect (msg)
               (signal 'redirect :status-code 303 :location (register-path :err msg)))
             (param (name)
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
                          (print "diff passwords")
                          (redirect "Passwords are not the same.")))))
        (print "=PAYLOAD=")
        (print payload)
        (services:register-user user-dto)
        (redirect "Registered!")))))

(defmethod explain-condition :around ((c redirect)
                                      (resource (eql #'register-post))
                                      (ct snooze-types:application/x-www-form-urlencoded))
  (setf (hunchentoot:header-out :location) (location c))
  (print "===========")
  (format t "~A" (location c))
  (print "===========")
  (format nil "See here: ~a" (location c)))

(defroute home
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
