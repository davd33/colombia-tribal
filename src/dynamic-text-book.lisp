(in-package #:dynamic-text-book)

(defvar *line-cursor* nil)

(defvar *in* nil)

(defconstant top-level-line "* ")

(defconstant story-line "* STORY ")

(defconstant actions-line "** ACTIONS")

(defconstant action-line "* ACTION ")

(defmacro next-line (var &optional (default :none))
  "SETF VAR reading a line from *in*
RESTART-CASES:
 - skip, returns nil"
  `(restart-case (setf ,var ,(if (eq :none default)
                                 (print `(read-line *in*))
                                 (print `(read-line *in* nil))))
     (use-eof () (setf ,var :end-of-file))
     (skip () nil)
     (use-value (v) v)))

(defun make-story (text &optional action-buttons)
  "Create a story."
  (list text
        action-buttons))

(defun story-text (story)
  "Return text bit of a STORY."
  (first story))

(defun story-action-buttons (story)
  "Return action buttons of STORY."
  (second story))

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

(defun make-action (text)
  "Create an action.
An action is a bit of text that explains the transition to another story."
  (list text))

(defun action-text (action)
  "Return text bit of ACTION"
  (first action))

(defun add-text-to-action (text action)
  "Return updated ACTION with TEXT added."
  (list (str:concat (action-text action)
                    text)))

(defun title->id (title)
  "Returns a dashed string of title."
  (str:concat (str:downcase (str:replace-all " " "-" title))
              "-story"))

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
                (next-story (if (and (or actions-p action-line-p)
                                     (not (str:emptyp line)))
                                (add-action-button-to-story
                                 (let ((action (mapcar #'str:trim (str:split "," line))))
                                   (multiple-value-bind (action-text quoted)
                                       (str:replace-all "\"" "" (first action))
                                     (make-action-button action-text
                                                         (title->id (second action))
                                                         (not quoted))))
                                 story)
                                (add-text-to-story line story))
                            action-line-p)))
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
          (next-action (add-text-to-action line action))))))

(defmacro build-book (dtb-path destination-package)
  "Build the whole dynamic text book from DTB-PATH."
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
                                    (let* ((symbol-str (title->id (second (str:split story-line *line-cursor*)))))
                                      `(hm:put ,st ,symbol-str ',(next-story))))
                                   ((str:starts-with-p action-line *line-cursor*)
                                    (let* ((symbol-str (title->id (second (str:split action-line *line-cursor*)))))
                                      `(hm:put ,ac ,symbol-str ',(next-action)))))))))
        `(progn
           (defpackage ,destination-package
             (:use #:cl)
             (:export #:|*stories*|
                      #:|*actions*|))
           (in-package ,destination-package)
           ,@list-of-definitions)))))
