(in-package #:game)

(eval-when
    (:compile-toplevel
     :load-toplevel)

  (defpackage :colombia-tribal-game
    (:use #:cl)
    (:export #:|*stories*|
             #:|*actions*|))

  (build-book "./src/resources/story/start.org" :colombia-tribal-game))
