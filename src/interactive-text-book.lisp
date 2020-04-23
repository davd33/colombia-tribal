(in-package #:interactive-text-book)

(define-condition not-a-package-error (error)
  ((text :initarg :text :reader text)))

(define-condition missing-export-error (error)
  ((text :initarg :text :reader text)))

(define-condition illegal-headline-value (error)
  ((text :initarg :text :reader text)))

(defvar *line-cursor* nil)

(defvar *in* nil)

(defparameter top-level-line "* ")

(defparameter story-line "* STORY ")

(defparameter actions-line "** ACTIONS")

(defparameter image-line "** IMAGE ")

(defparameter action-line "* ACTION ")

(defmacro next-line (var &optional (default :none))
  "SETF VAR reading a line from *in*
RESTART-CASES:
 - skip, returns nil"
  `(restart-case (setf ,var ,(if (eq :none default)
                                 `(read-line *in*)
                                 `(read-line *in* nil)))
     (use-eof () (setf ,var :end-of-file))
     (skip () nil)
     (use-value (v) v)))

(defun make-story (text &key action-buttons (image :no-image) id)
  "Create a story."
  (list text
        action-buttons
        image
        id))

(defun story-id (story)
  "Return identifier of STORY."
  (fourth story))

(defun story-text (story)
  "Return text bit of a STORY."
  (first story))

(defun story-action-buttons (story)
  "Return action buttons of STORY."
  (second story))

(defun story-image (story)
  "Return image of STORY."
  (third story))

(defun add-text-to-story (text story)
  "Return updated story with TEXT added.
Effect on STORY."
  (setf (nth 0 story) (str:concat (story-text story)
                                  text))
  story)

(defun add-action-button-to-story (action-button story)
  "Return updated story whit ACTION-BUTTON added.
Effect on STORY."
  (setf (nth 1 story) (append (story-action-buttons story)
                              (list action-button)))
  story)

(defun set-image-story (img-file-name story)
  "Return updated story with the given image file name.
 Effect on STORY."
  (setf (nth 2 story) img-file-name)
  story)

(defun set-id-story (id story)
  "Return updated story with ID.
Effect on STORY."
  (setf (nth 3 story) id)
  story)

(defun make-action-button (text destination-story &optional ref-p)
  "Create an action button.
If REF-P is not nil, text refers to an action function."
  (list text
        destination-story
        ref-p))

(defun make-action (text &optional image)
  "Create an action.
An action is a bit of text that explains the transition to another story."
  (list text
        image))

(defun action-text (action)
  "Return text bit of ACTION"
  (first action))

(defun action-image (action)
  "Return image name of ACTION."
  (second action))

(defun set-image-action (img-file-name action)
  "Return updated action with the given image file name."
  (setf (nth 1 action) img-file-name)
  action)

(defun add-text-to-action (text action)
  "Return updated ACTION with TEXT added."
  (setf (nth 0 action) (str:concat (action-text action) text))
  action)

(defun id->title (id)
  "Returns a capitalized title."
  (str:capitalize
   (str:replace-all
    "-" " "
    (str:replace-all
     "-action" ""
     (str:replace-all "-story" "" id)))))

(defun title->id (title suffix)
  "Returns a dashed string of title."
  (str:concat (str:downcase (str:replace-all " " "-" title))
              suffix))

(defun make-headline (line)
  "Returns a list of three elements that make a head line.
A headline always contains three components:
 - the type (e.g. ACTION or STORY),
 - the Title and/or ID if no specific ID is given,
 - the ID (keyword)."
  (let ((headline (mapcar #'str:trim (str:split "," (subseq line 1)))))
    (assert (<= (length headline) 3))
    headline))

(defun id-provided-p (headline)
  "For any headline: return T when a specific ID is give."
  (and (third headline)
       (not (str:empty? (third headline)))))

(defun type-headline (headline)
  "Return the ID of the headline."
  (str:downcase (first headline)))

(defun title-headline (headline)
  "Return the title of the headline."
  (second headline))

(defun id-headline (headline)
  "Return the ID of the headline."
  (if (id-provided-p headline)
      (third headline)
      (title->id (title-headline headline) (str:concat "-" (type-headline headline)))))

(defun next-story (story &optional actions-p)
  "Reads *IN* and update *LINE-CURSOR* with NEXT-LINE line by line.
If ACTIONS-P is not nil, it means that we are collecting action lines."
  (handler-bind ((end-of-file #'(lambda (c)
                                  (format t "~&WARNING: NEXT-STORY - ~A~%" c)
                                  (invoke-restart 'use-eof))))
    (let ((line (next-line *line-cursor*)))
      (if (not (eq :end-of-file line))
          (let* ((action-line-p (str:starts-with-p actions-line line))
                 (line (or (and action-line-p (next-line *line-cursor* nil))
                           line)))
            (if (str:starts-with-p top-level-line line)
                ;; we reached the next section
                story
                ;; we continue building the current story
                (next-story (cond (;; IMAGE FILE NAME
                                   (str:starts-with-p image-line line)
                                   (set-image-story (str:replace-all image-line "" line) story))

                                  (;; ACTION-BUTTON
                                   (and (or actions-p action-line-p)
                                        (not (str:emptyp line)))
                                   (add-action-button-to-story
                                    (let ((action (mapcar #'str:trim (str:split "," line))))
                                      (multiple-value-bind (action-text quoted)
                                          (str:replace-all "\"" "" (first action))
                                        (make-action-button action-text
                                                            (title->id (second action)
                                                                       "-story")
                                                            (not quoted))))
                                    story))

                                  (;; STORY TEXT
                                   t (add-text-to-story (if (str:emptyp line)
                                                            "<br/>"
                                                            (str:concat " " line))
                                                        story)))
                            (or action-line-p actions-p))))
          story))))

(defun next-action (action)
  "Reads *IN* and update *LINE-CURSOR* with NEXT-LINE line by line."
  (handler-bind ((end-of-file #'(lambda (c)
                                  (format t "~&WARNING: NEXT-ACTION - ~A~%" c)
                                  (invoke-restart 'use-eof))))
    (let ((line (next-line *line-cursor*)))
      (if (or (eq :end-of-file line)
              (str:starts-with-p top-level-line line))
          action
          (next-action (cond (;; IMAGE FILE NAME
                              (str:starts-with-p image-line line)
                              (set-image-action (str:replace-all image-line "" line) action))
                             (;; ACTION TEXT
                              t (add-text-to-action line action))))))))

(defun symbol-of (str package)
  "Return a symbol named STR interned in PACKAGE."
  (intern str package))

(defun has-symbols (symbols package)
  "Return T when all strings in SYMBOLS are defined in PACKAGE (packagep)."
  (not (position
        nil
        (mapcar #'(lambda (s) (find-symbol s package))
                symbols))))

(defmacro defbook (dtb-path destination-package)
  "Build the whole dynamic text book from DTB-PATH.
DESTINATION-PACKAGE must be a package-designator to an existing package so that.
Otherwise the condition 'not-a-package will be signaled.
It must as well export the following two symbols: #:|*stories*| and #:|*actions*|."
  (cond
    ((not (packagep (find-package destination-package)))
     (error 'not-a-package-error :text "DESTINATION-PACKAGE must be an existing package."))
    ((not (has-symbols '("*stories*" "*actions*")
                       destination-package))
     (error 'missing-export-error :text "DESTINATION-PACKAGE must export |*stories*| and |*actions*|."))
    (t
     (with-open-file (*in* dtb-path)
       (symbol-macrolet ((st (intern "*stories*" destination-package))
                         (ac (intern "*actions*" destination-package))
                         (headline (make-headline *line-cursor*)))
         (let* ((*line-cursor* (read-line *in*))
                (list-of-definitions
                 `((defvar ,st (make-hash-table :test 'equal))
                   (defvar ,ac (make-hash-table :test 'equal))
                   ,@(loop
                        until (eq *line-cursor* :end-of-file)
                        collect (cond (;; STORY
                                       (string= "story" (type-headline headline))
                                       (let* ((symbol-str (id-headline headline)))
                                         `(hm:put ,st ,symbol-str ',(next-story
                                                                     (make-story "" :id symbol-str)))))
                                      (;; ACTION
                                       (string= "action" (type-headline headline))
                                       (let* ((symbol-str (id-headline headline)))
                                         `(hm:put ,ac ,symbol-str ',(next-action (make-action ""))))))))))
           `(progn
              (in-package ,destination-package)
              ,@list-of-definitions)))))))
