(defpackage #:alists
  (:use #:cl #:alexandria)
  (:export #:aconses
           #:deep-acons
           #:merge-acons))

(defpackage #:jsons
  (:use #:cl)
  (:export #:get-in
           #:add-value
           #:type-compatible-p))

(defpackage #:hm
  (:use #:cl)
  (:shadow #:get
           #:reduce
           #:first)
  (:export #:put
           #:get
           #:one
           #:reduce
           #:print-elt
           #:print-all))

(defpackage #:data
  (:use #:cl #:alexandria)
  (:export #:group-by))

(defpackage #:memoize
  (:use #:cl #:alexandria)
  (:export #:memo
           #:memoize
           #:clear-memoize
           #:defmemo))

(defpackage #:pipe
  (:use #:cl)
  (:export #:delay
           #:force
           #:make-pipe
           #:empty-pipe
           #:head
           #:tail
           #:pipe-elt
           #:integers
           #:foreach))

(defpackage #:mop
  (:use #:cl #:alexandria)
  (:export #:make-mapper
           #:find-class-slots
           #:class-slots
           #:defprintobj
           #:with-computed-slot
           #:with-mapped-slot
           #:with-renamed-slot))

(defpackage #:dynamic-text-book
  (:use #:cl #:alexandria)
  (:export #:build-book
           #:make-story
           #:story-text
           #:story-action-buttons
           #:add-text-to-story
           #:add-action-button-to-story
           #:make-action-button
           #:make-action
           #:action-text
           #:add-text-to-action
           #:title->id))

(defpackage #:game
  (:use #:cl #:dynamic-text-book)
  (:export #:compile-colombia-tribal))

(defpackage #:dao
  (:use #:cl)
  (:export
   ;; ENTITIES
   ;; ENTITY FIELDS
   ;; RETRIEVE
   ;; INSERT
   ;; CONNECT AND CREATE TABLES
   #:connect
   #:*connection*))

(defpackage #:dto
  (:use #:cl)
  (:export ;; DTOs
           ;; ACCESSORS
   ))

(defpackage #:api
  (:use #:cl #:snooze #:jsons #:alexandria)
  (:export #:start
           #:stop))

(defpackage #:html
  (:use #:cl #:spinneret #:alexandria)
  (:export #:story->html
           #:action->html))

(defpackage #:web-site
  (:use #:cl #:snooze #:jsons #:alexandria)
  (:export #:home))

(defpackage #:services
  (:use #:cl #:jsons #:alexandria #:spinneret)
  (:export #:get-story))
