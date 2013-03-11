(defpackage :cl-async-future
  (:use :cl)
  (:nicknames :asf)
  (:export #:future
           #:make-future
           #:attach-errback
           #:signal-error
           #:futurep
           #:finish
           #:attach
           #:alet
           #:alet*
           #:multiple-future-bind
           #:wait-for
           #:future-handler-case))

