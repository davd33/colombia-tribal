(in-package :html)

(defparameter *page-title* "Colombia Tribal")

(defmacro css (&body styles)
  "Takes 1..n CSS instructions as 2-elements lists, then returns a css formatted string.
   A CSS instruction list looks like this: (:font-size <string>)"
  `(str:concat
     ,@(loop for style in styles
          collect `(format nil "~a: ~a;~%" ,(string (first style)) ,(second style)))))

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:link :href "/css/main.css" :rel "stylesheet" :type "text/css")
       (:title ,title))
      (:body
       ,@body))))

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

(defun action->html (action action-title story-destination)
  "Converts an action to html."
  (with-page (:title action-title)
    (:div.action
     (repeat
       :for (p (str:split "<br/>" (dynamic-text-book:action-text action)))
       (:p p))
     (:p (link :href (str:concat "/story/" story-destination)
               "Continue")))))

(defun story->html (story story-title)
  "Converts an story to html."
  (with-page (:title story-title)
    (:div.story
     (repeat
       :for (p (str:split "<br/>" (dynamic-text-book:story-text story)))
       (:p p))
     (:div.action-buttons
      (repeat
        :for (action-button (dynamic-text-book:story-action-buttons story))
        (destructuring-bind (action-title story-destination indirection)
            action-button
          (if indirection
              (:p (link :href (str:concat "/action/" (dynamic-text-book:title->id
                                                      action-title)
                                          "/" story-destination)
                        action-title))
              (:p (link :href (str:concat "/story/" story-destination)
                        action-title)))))))))
