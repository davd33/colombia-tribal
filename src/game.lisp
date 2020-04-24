(in-package #:game)

(eval-when
    (:compile-toplevel
     :load-toplevel)

  (defpackage :colombia-tribal-game
    (:use #:cl)
    (:export #:|*stories*|
             #:|*actions*|))

  (defbook (resource "story/start.org") :colombia-tribal-game))
