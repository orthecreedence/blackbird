(defpackage :blackbird
  (:use :cl)
  (:nicknames :bb)
  (:export #:promise
           #:promise-finished-p
           #:make-promise
           #:with-promise
           #:promisify
           
           #:attach-errback
           #:lookup-forwarded-promise
           #:signal-error
           #:promisep
           #:finish
           #:reset-promise

           #:attach
           #:catcher
           #:finally

           #:chain
           #:alet
           #:alet*
           #:aif
           #:multiple-promise-bind
           #:wait
           #:adolist

           #:promise-handler-case

           #:*promise-keep-specials*
           #:*promise-finish-hook*))
