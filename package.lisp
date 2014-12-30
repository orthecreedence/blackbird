(defpackage :blackbird-base
  (:use :cl)
  (:export #:*debug-on-error*
           #:*promise-keep-specials*
           #:*promise-finish-hook*

           #:promise
           #:promise-finished-p
           #:make-promise
           #:create-promise
           #:with-promise
           #:promisify
           
           #:attach-errback
           #:lookup-forwarded-promise
           #:signal-error
           #:promisep
           #:finish
           #:reset-promise

           #:do-attach
           #:attach
           #:do-catch
           #:catcher
           #:do-tap
           #:tap
           #:do-finally
           #:finally))

(defpackage :blackbird-syntax
  (:use :cl :blackbird-base)
  (:nicknames :bs)
  (:export #:alet
           #:alet*
           #:aif
           #:multiple-promise-bind
           #:wait))

(defpackage :blackbird-util
  (:use :cl :blackbird-base :blackbird-syntax)
  (:export #:adolist
           #:amap
           #:all
           #:areduce
           #:afilter
           #:tap
           #:chain))

(defpackage :blackbird
  (:use :cl :blackbird-base :blackbird-syntax :blackbird-util)
  (:nicknames :bb)
  (:export #:*debug-on-error*
           #:*promise-keep-specials*
           #:*promise-finish-hook*

           #:promise
           #:promise-finished-p
           #:create-promise
           #:with-promise
           #:promisify
           
           #:attach-errback
           #:signal-error
           #:promisep

           #:attach
           #:catcher
           #:tap
           #:finally

           #:alet
           #:alet*
           #:aif
           #:multiple-promise-bind
           #:wait

           #:adolist
           #:amap
           #:all
           #:areduce
           #:afilter
           #:chain))

