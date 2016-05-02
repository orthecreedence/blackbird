(in-package :blackbird-util)

(defun aeach (function promise-list)
  "Given a function and a list of values/promises, runs the function on each
   resolved promise *in sequence*."
  (with-promise (resolve reject :name "each")
    (labels ((next (promise-list)
               (if (zerop (length promise-list))
                   ;; all done
                   (resolve)
                   ;; grab the next promise/val off the list, resolve it, give
                   ;; it to our heroic function, and wait for the return value/
                   ;; promise (then continue)
                   (catcher
                     (attach (car promise-list)
                       (lambda (&rest vals)
                         (wait (apply function vals)
                           (next (cdr promise-list)))))
                     (condition (e) (reject e))))))
      (next promise-list))))

(defmacro adolist ((item items &optional promise-bind) &body body)
  "Async version of dolist, only continues loop when promise in final form
   finishes with a value."
  (when promise-bind
    (format t "<WARN> blackbird: using adolists's third param (promise-bind) is deprecated.~%"))
  (let ((rest-sym (gensym "resttt")))
    `(aeach
       (lambda (&rest ,rest-sym)
         (let ((,item (car ,rest-sym)))
           ,@body))
       ,items)))

(defun amap (function promise-list)
  "Run map over a list of promises/values, finishing the returned promise once
   all values have been fulfilled. If the given function returns a promise, its
   value will be used in the return array.
   
   If any of the given promises fail, the main returned promise fails."
  (with-promise (resolve reject :name "amap")
    (catcher
      (alet* ((promise-list promise-list)
              (vals (make-array (length promise-list)))
              (num-returned 0))
        (if (null promise-list)
            (resolve nil)
            (loop for i from 0
                  for promise in promise-list do
                    (let ((idx i))
                      (catcher
                       (progn
                         (attach-errback promise (lambda (e) (reject e)))
                         (alet* ((val promise)
                                 (val (funcall function val)))
                           (setf (aref vals idx) val)
                           (incf num-returned)
                           (when (<= (length vals) num-returned)
                             (resolve (coerce vals 'list)))))
                       (condition (e) (reject e)))))))
      (condition (e) (reject e)))))

(defun all (promise-list)
  "Wait for all of the promises in the given list to resolve before resolving.
   If an error occurs on any of the given promises, it is forwarded to all's
   returned promise."
  (with-promise (resolve reject :name "all")
    (resolve (amap 'identity promise-list))))

(defun areduce (function promise-list &optional initial-value)
  "Perform a reduce on a list of promises, or a promise of a list of promises,
   resolving the returned promise once complete."
  (with-promise (resolve reject :name "areduce")
    (catcher
      (alet* ((res (all promise-list)))
        (catcher
          (resolve (reduce function res :initial-value initial-value))
          (condition (e) (reject e))))
      (condition (e) (reject e)))))

(defun afilter (function promise-list)
  "Perform a filter on a list of promises, or a promise of a list of promises,
   resolving the returned promise with the filtered list once complete."
  (with-promise (resolve reject :name "afilter")
    (catcher
      (alet* ((res (all promise-list))
              (res (amap (lambda (x)
                           (alet* ((val (funcall function x)))
                             (and val x)))
                         res)))
        (catcher
          (resolve (remove-if 'null res))
          (condition (e) (reject e))))
      (condition (e) (reject e)))))

(defmacro chain (promise-gen &body operations)
  "Chain a number of promise operations together in an easy to read way:

     (chain (+ 4 5)
       (:then (x) (values (/ x 3) 69))
       (:then (x y) (list 4 (* x y)))
       (:map (x) (+ x 42))
       (:reduce (acc val 0) (+ acc val))
       (:then (x) (send-x-to-server x))
       (:catch (e) (my-error-handler e))
       (:finally () (we-are-done)))

   Allowed operations are
    - :then/:attach
    - :map
    - :reduce
    - :filter
    - :all
    - :catch/:catcher
    - :finally"
  (flet ((transform-op (promise op)
           (case (car op)
             ((:then :attach)
              `(multiple-promise-bind ,(cadr op) ,promise ,@(cddr op)))
             (:map
              `(amap (lambda ,(cadr op) ,@(cddr op)) ,promise))
             (:reduce
              (let ((args (cadr op)))
                `(areduce (lambda ,(list (car args) (cadr args)) ,@(cddr op)) ,promise ,(caddr args))))
             (:filter
              `(afilter (lambda ,(cadr op) ,@(cddr op)) ,promise))
             (:all
              `(all ,promise))
             ((:catch :catcher)
              `(do-catch ,promise (lambda ,(cadr op) ,@(cddr op))))
             (:finally
              `(finally ,promise ,@(cdr op)))
             (t (error (format nil "invalid chain operation: ~a" (car op)))))))
    (let* ((chain (transform-op `(promisify ,promise-gen) (car operations))))
      (dolist (op (cdr operations))
        (setf chain (transform-op `(promisify ,chain) op)))
      chain)))

