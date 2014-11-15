(defpackage :blackbird
  (:use :cl)
  (:nicknames :asf :bb)
  (:export #:promise
           #:promise-finished-p
           #:make-promise
           #:attach-errback
           #:lookup-forwarded-promise
           #:signal-error
           #:promisep
           #:finish
           #:reset-promise
           #:attach
           #:alet
           #:alet*
           #:aif
           #:multiple-promise-bind
           #:wait
           #:adolist
           #:promise-handler-case
    
           ;; cl-async-future compatibility classes/functions/macros
           #:future
           #:future-finished-p
           #:make-future
           #:lookup-forwarded-future
           #:futurep
           #:reset-future
           #:multiple-future-bind
           #:future-handler-case
           #:wait-for))

