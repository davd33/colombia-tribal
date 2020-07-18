(in-package :html)

(defparameter *page-title* "Colombia Tribal")

(defmacro css (&body styles)
  "Takes 1..n CSS instructions as 2-elements lists, then returns a css formatted string.
   A CSS instruction list looks like this: (:font-size <string>)"
  `(str:concat
     ,@(loop for style in styles
          collect `(format nil "~a: ~a;~%" ,(string (first style)) ,(second style)))))

(defmacro with-page ((&key title image-path) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:link :href "/css/main.css" :rel "stylesheet" :type "text/css")
       (:title ,title)
       (when ,image-path
         (:style "html {"
                 (css (:background (str:concat "url(" ,image-path ")")))
                 "background-size: cover;"
                 "background-repeat: no-repeat;"
                 "}")))
      (:body
       (:div.container ,@body)))))

(deftag link (text attrs &key href class)
  `(:a.contact-link
    :class ,class
    :href ,href
    ,@attrs
    ,@text))

(deftag repeat (template attrs &key for)
  "This is a tag that repeats a given template using the key
for a translation split into a list of several strings.
  - for: lang-binding-form: 2 elements list with var name and translation key
  - template: a single form (one list of potentially embedded tags)"
  `(reduce #'(lambda (acc elt)
               (append
                acc
                (let ((,(caadr for) elt))
                  ,@template)))
           ,@(cdadr for)
           :initial-value `(progn)))

(defun action->html (action action-title story-destination action-image)
  "Converts an action to html."
  (with-page (:title action-title :image-path "/images/the-cavern.jpg")
    (:h1 action-title)
    (:div.action
     (repeat
       :for (p (str:split "<br/>" (interactive-text-book:action-text action)))
       (:p p))
     (:p (link :href (str:concat "/story/" story-destination)
               "Continue")))))

(defun story->html (story story-title story-image)
  "Converts an story to html."
  (with-page (:title story-title :image-path "/images/the-cavern.jpg")
    (:h1 story-title)
    (:div.story
     (repeat
       :for (p (str:split "<br/>" (interactive-text-book:story-text story)))
       (:p p))
     (:div.action-buttons
      (repeat
        :for (action-button (interactive-text-book:story-action-buttons story))
        (destructuring-bind (action-title story-destination indirection)
            action-button
          (if indirection
              (:p (link :href (str:concat "/action/" (interactive-text-book:title->id
                                                      action-title "-action")
                                          "/" story-destination)
                        action-title))
              (:p (link :href (str:concat "/story/" story-destination)
                        action-title)))))))))

(defun home (title)
  "Home of the web site."
  (with-page (:title title :image-path "/images/the-cavern.jpg")
    (:h1 title)
    (:p "Welcome to the website where you can write your own interactive books and play the ones writen by the community.")
    (:p "Wanna have a look how an interactive book looks like: "
        (link :href "/colombia-tribal" "this way!"))
    (:p "Wanna register and start writing your own interactive books: "
        (link :href "/register" "down here!"))
    (:br)
    (link :href "/login" "Sign in!")))

(defun logged-in-index (pseudo)
  "You're logged in!"
  (with-page (:title (str:concat pseudo "You're now logged-in!"))
    (:h1 "Welcome to the site, you can now create your own stories!")))

(defun login-form (action err-msg)
  "Form to log in"
  (with-page (:title "Log in" :image-path "/images/the-cavern.jpg")
    (:h1 "Please fill in your credentials")
    (:p err-msg)
    (:form :method "POST"
           :action action
           (:input :type "text"
                   :name "pname"
                   :placeholder "Username")
           (:br)
           (:input :type "password"
                   :name "password"
                   :placeholder "Password")
           (:br)
           (:input :type "submit" :value "Sign in"))))

(defun register-form (title action err-msg)
  "Form for registering."
  (with-page (:title title :image-path "/images/the-cavern.jpg")
    (:h1 title)
    (:p err-msg)
    (:form :method "POST"
           :action action
           (:input :type "text"
                   :name "pname"
                   :placeholder "Davd33")
           (:br)
           (:input :type "password"
                   :name "password"
                   :placeholder "Password")
           (:input :type "password"
                   :name "password_repeat"
                   :placeholder "Repeat Password")
           (:br)
           (:input :type "text"
                   :name "mail"
                   :placeholder "Mail (not mandatory)")
           (:br)
           (:input :type "submit" :value "Create"))))
