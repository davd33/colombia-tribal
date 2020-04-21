(in-package #:dynamic-text-book)

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

(defun make-story (text &optional action-buttons image)
  "Create a story."
  (list text
        action-buttons
        image))

(defun story-text (story)
  "Return text bit of a STORY."
  (first story))

(defun story-action-buttons (story)
  "Return action buttons of STORY."
  (second story))

(defun story-image (story)
  "Return image of STORY."
  (third story))

(defun set-image-story (img-file-name story)
  "Return updated story with the given image file name."
  (list (story-text story)
        (story-action-buttons story)
        img-file-name))

(defun add-text-to-story (text story)
  "Return updated story with TEXT added."
  (list (str:concat (story-text story)
                    text)
        (story-action-buttons story)))

(defun add-action-button-to-story (action-button story)
  "Return updated story whit ACTION-BUTTON added."
  (list (story-text story)
        (append (story-action-buttons story)
                (list action-button))))

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
  (list (action-text action)
        img-file-name))

(defun add-text-to-action (text action)
  "Return updated ACTION with TEXT added."
  (list (str:concat (action-text action)
                    text)))

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

(defun next-story (&optional story actions-p)
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

(defun next-action (&optional action)
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

(define-condition not-a-package-error (error)
  ((text :initarg :text :reader text)))

(define-condition missing-export-error (error)
  ((text :initarg :text :reader text)))

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
                         (ac (intern "*actions*" destination-package)))
         (let* ((*line-cursor* (read-line *in*))
                (list-of-definitions
                 `((defvar ,st (make-hash-table :test 'equal))
                   (defvar ,ac (make-hash-table :test 'equal))
                   ,@(loop
                        until (eq *line-cursor* :end-of-file)
                        collect (cond ((str:starts-with-p story-line *line-cursor*)
                                       (let* ((symbol-str (title->id (second (str:split story-line *line-cursor*))
                                                                     "-story")))
                                         `(hm:put ,st ,symbol-str ',(next-story))))
                                      ((str:starts-with-p action-line *line-cursor*)
                                       (let* ((symbol-str (title->id (second (str:split action-line *line-cursor*))
                                                                     "-action")))
                                         `(hm:put ,ac ,symbol-str ',(next-action)))))))))
           `(progn
              (in-package ,destination-package)
              ,@list-of-definitions)))))))
