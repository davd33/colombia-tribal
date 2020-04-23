(in-package #:web-site)

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

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
                        (interactive-text-book:id->title (str:downcase story-id))
                        (interactive-text-book:story-image story)))))

(defroute action
  (:get "text/html" action-id story-id)
  (build-spinneret-html-response
    (let ((action (hm:get colombia-tribal-game:|*actions*| (str:downcase action-id))))
      (html:action->html action
                         (interactive-text-book:id->title (str:downcase action-id))
                         (str:downcase story-id)
                         (interactive-text-book:action-image action)))))
