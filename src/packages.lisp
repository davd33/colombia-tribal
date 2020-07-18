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

(defpackage #:resources
  (:use #:cl)
  (:export #:*profile*
           #:*system*
           #:resource))

(defpackage #:interactive-text-book
  (:use #:cl #:alexandria)
  (:export #:defbook
           #:make-story
           #:story-title
           #:story-text
           #:story-image
           #:story-action-buttons
           #:add-text-to-story
           #:add-action-button-to-story
           #:make-action-button
           #:make-action
           #:action-text
           #:action-image
           #:add-text-to-action
           #:title->id
           #:id->title))

(defpackage #:game
  (:use #:cl #:interactive-text-book #:resources)
  (:export #:compile-colombia-tribal))

(defpackage #:dao.utils
  (:use #:cl)
  (:export #:connect
           #:*connection*
           #:make-mapper
           #:json->dao-mapper
           #:json->dao-mapper-kind
           #:json->dao-mapper-hm
           #:defmapper
           #:json->dao
           #:create-table
           #:drop-table
           #:create-tables
           #:drop-tables
           #:reset-db-tables))

(defpackage #:dao
  (:use #:cl #:dao.utils)
  (:export
   ;; ENTITIES
   #:user
   #:login
   ;; ENTITY FIELDS
   #:pname
   #:password
   #:mail
   #:token
   #:expires
   ;; RETRIEVE
   #:retrieve-user
   #:retrieve-login-by-user-id
   ;; INSERT
   #:insert-user
   #:insert-login
   ;; AUTH FEATURES
   #:verify-login
   ;; CONNECT AND CREATE TABLES
   #:connect
   #:*connection*))

(defpackage #:dto
  (:use #:cl)
  (:export #:user
           #:login
           ;; ACCESSORS
           #:pname
           #:password
           #:mail
           #:token
           #:expires))

(defpackage #:api
  (:use #:cl #:snooze #:jsons #:alexandria #:resources)
  (:export #:start
           #:stop))

(defpackage #:html
  (:use #:cl #:spinneret #:alexandria)
  (:export #:story->html
           #:action->html
           #:login-form
           #:logged-in-index
           #:register-form
           #:home))

(defpackage #:web-site
  (:use #:cl #:snooze #:jsons #:alexandria)
  (:export #:home))

(defpackage #:services
  (:use #:cl #:jsons #:alexandria #:spinneret)
  (:export #:register-user))
