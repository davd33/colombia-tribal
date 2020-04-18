(in-package #:dynamic-text-book)

(defvar *line-cursor* nil)

(defvar *in* nil)

(defconstant top-level-line "* ")

(defconstant story-line "* STORY ")

(defconstant actions-line "** ACTIONS")

(defconstant action-line "* ACTION ")

(defmacro next-line (var)
  "SETF VAR reading a line from *in*
RESTART-CASES:
 - skip, returns nil"
  `(restart-case (setf ,var (read-line *in*))
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

(defun make-action (text destination-story)
  "Create an action.
An action is a bit of text that explains the transition to another story."
  (list text
        destination-story))

(defun next-story (&optional story actions-p)
  "Reads *IN* and update *LINE-CURSOR* with NEXT-LINE line by line.
If ACTIONS-P is not nil, it means that we are collecting action lines."
  (handler-bind ((end-of-file #'(lambda (c)
                                  (format t "~&WARNING: NEXT-STORY - ~A~%" c)
                                  (invoke-restart 'skip))))
    (let* ((line (next-line *line-cursor*)))
      (when line
        (let ((action-line-p (str:starts-with-p actions-line line)))
          (if (str:starts-with-p top-level-line line)
              ;; we reached the next section
              story
              ;; we continue building the current story
              (next-story (if actions-p
                              (add-action-button-to-story
                               (let ((action (mapcar #'str:trim (str:split "," line))))
                                 (multiple-value-bind (action-text quoted)
                                     (str:replace-all "\"" "" (first action))
                                   (make-action-button action-text
                                                       (second action)
                                                       (not quoted))))
                               story)
                              (add-text-to-story line story))
                          action-line-p)))))))

(defmacro build-book (dtb-path)
  "Build the whole dynamic text book from DTB-PATH."
  (with-open-file (*in* dtb-path)
    (let ((*line-cursor* nil))
      (handler-bind ((end-of-file #'(lambda (c)
                                      (format t "~&WARNING: BUILD-BOOK - ~A~%" c)
                                      (use-value :end-of-file))))
        (loop for l = (next-line *line-cursor*)
           until (eq l :end-of-file)
           collect (cond ((str:starts-with-p story-line l)
                          `(defparameter
                               ,(format nil "~A-story"
                                        (second (str:split story-line l)))
                             ',(next-story)))))))))
