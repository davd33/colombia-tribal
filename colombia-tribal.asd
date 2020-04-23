(defsystem "colombia-tribal"
  :version "0.1.0"
  :author "David Rueda / Maria Jose Alarcon Albornoz"
  :license "GPLv3"
  :serial t
  :depends-on (#:spinneret
               #:hunchentoot
               #:snooze
               #:dexador
               #:cl-json
               #:clack
               #:fset
               #:str
               #:mito
               #:sxql
               #:unix-opts
               #:trivia
               #:alexandria
               #:closer-mop)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "alists")
                 (:file "jsons")
                 (:file "hm")
                 (:file "data")
                 (:file "memoize")
                 (:file "pipe")
                 (:file "mop")
                 (:file "interactive-text-book")
                 (:file "game")
                 (:file "dao")
                 (:file "dto")
                 (:file "api")
                 (:file "html")
                 (:file "web-site")
                 (:file "services"))))
  :description "The Free custom CV compatible with all other CV/jobs plateform."
  :in-order-to ((test-op (test-op "colombia-tribal/tests"))))

(defsystem "colombia-tribal/tests"
  :author "David Rueda / Maria Jose Alarcon Albornoz"
  :license "GPLv3"
  :depends-on ("colombia-tribal"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "jsons")
                 (:file "api-handlers"))))
  :description "Test system for colombia-tribal"
  :perform (test-op (op c) (symbol-call :rove :run c)))
