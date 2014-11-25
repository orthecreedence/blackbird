(defpackage :blackbird
  (:use :cl)
  (:nicknames :bb)
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
           #:promise-handler-case))

