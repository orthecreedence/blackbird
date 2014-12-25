(in-package :blackbird-util)

(defmacro adolist ((item items &optional promise-bind) &body body)
  "Async version of dolist, only continues loop when promise in final form
   finishes with a value."
  (let ((items-sym (gensym "items"))
        (promise-sym (gensym "promise"))
        (next-fn (gensym "next-fn")))
    `(let ((,items-sym ,items)
           (,promise-sym (make-promise :name "adolist")))
       (labels ((,next-fn ()
                  (let ((,item (car ,items-sym)))
                    (unless ,item
                      (finish ,promise-sym nil)
                      (return-from ,next-fn))
                    (setf ,items-sym (cdr ,items-sym))
                    (catcher
                      ,(if promise-bind
                           `(let ((,promise-bind (make-promise)))
                              (wait ,promise-bind (,next-fn))
                              ,@body)
                           `(wait (progn ,@body) (,next-fn)))
                      (t (e) (signal-error ,promise-sym e))))))
         (,next-fn))
       ,promise-sym)))

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
              (condition (e) (reject e))))))
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
             ((or :then :attach)
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
             ((or :catch :catcher)
              `(do-catch ,promise (lambda ,(cadr op) ,@(cddr op))))
             (:finally
              `(finally ,promise ,@(cdr op)))
             (t (error (format nil "invalid chain operation: ~a" (car op)))))))
    (let* ((chain (transform-op `(promisify ,promise-gen) (car operations))))
      (dolist (op (cdr operations))
        (setf chain (transform-op `(promisify ,chain) op)))
      chain)))

