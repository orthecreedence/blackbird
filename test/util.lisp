(defpackage :blackbird-test
  (:use :cl :fiveam :cl-async-base :cl-async-util :blackbird)
  (:shadow blackbird:*debug-on-error*)
  (:export #:run-tests))
(in-package :blackbird-test)

(defmacro async-let ((&rest bindings) &body body)
  "Wrap an async op inside of a let/start-event-loop block to mimick a blocking
   action. Bindings must be set from withing the block via setf."
  `(let ,bindings
     (as:start-event-loop
       (lambda ()
         ,@body)
       :catch-app-errors t)
     (values ,@(loop for (binding . nil) in bindings collect binding))))

(defun concat (&rest args)
  "Shortens string concatenation because I'm lazy and really who the hell wants
   to type out (concatenate 'string ...) seriously, I mispell concatentate like
   90% of the time I type it out."
  (apply #'concatenate (append '(string) args)))

(defun id (val)
  "Returns val. Yes, yes. I know that the identity function exists, but
   seriously I'm not going to waste precious time out of my life typing identity
   when I can just type id. The idea is the same, and everybody KNOWS what I'm
   trying to express.
   
   Oh one more thing: the only reason I NEED identi...err, id is because Eos
   can't use its `is` macro around another macro. So I need a function to wrap
   it. Lame. BUT such is life."
  val)
  
;; define the test suite
(def-suite blackbird-test :description "blackbird test suite")

