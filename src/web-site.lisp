(in-package #:web-site)

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(defroute home
  (:get "text/html")
  (build-spinneret-html-response
    (let ((intro-story
           (hm:get colombia-tribal-game:|*stories*| "Intro-story")))
      (html:story->html intro-story "Introduction"))))

(defroute story
  (:get "text/html" story-id)
  (build-spinneret-html-response
    (let ((story (hm:get colombia-tribal-game:|*stories*| (str:downcase story-id))))
      (html:story->html story story-id))))
