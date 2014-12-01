(in-package :blackbird)

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
              `(attach ,promise (lambda ,(cadr op) ,@(cddr op))))
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
              `(finally ,promise ,@(cddr op)))
             (t (error (format nil "invalid chain operation: ~a" (car op)))))))
    (let* ((chain (transform-op `(promisify ,promise-gen) (car operations))))
      (dolist (op (cdr operations))
        (setf chain (transform-op `(promisify ,chain) op)))
      chain)))

(defmacro alet (bindings &body body)
  "Asynchronous let. Allows calculating a number of values in parallel via
   promises, and runs the body when all values have computed with the bindings
   given available to the body.

   Also returns a promise that fires with the values returned from the body form,
   which allows arbitrary nesting to get a final value(s)."
  (let* ((ignore-bindings nil)
         (bindings (loop for (bind form) in bindings
                         collect (list (if bind
                                           bind
                                           (let ((igsym (gensym "alet-ignore")))
                                             (push igsym ignore-bindings)
                                             igsym))
                                       form)))
         (bind-vars (loop for (bind nil) in bindings collect bind))
         (promise (gensym "promise"))
         (num-bindings (gensym "num-bindings"))
         (finished-promise (gensym "finished-promise"))
         (finished-vals (gensym "finished-vals"))
         (finished-cb (gensym "finished-cb"))
         (args (gensym "args")))
    `(let* ((,num-bindings ,(length bindings)) ; make a local var for num-bindings
            (,finished-promise (make-promise))
            (,finished-vals nil)
            (,finished-cb
              ;; the hash table makes sure that *all* promises have fires at
              ;; least once. used to be a simple counter, but that doesn't
              ;; account for a promise firiing multiple times before its alet
              ;; brethren get a chance to finish.
              (let ((track-promise-fired (make-hash-table :test #'eq :size ,num-bindings)))
                (lambda (promise)
                  (if (promisep promise)
                      ;; mark this promise as finished in the tracking has
                      (setf (gethash promise track-promise-fired) t)
                      ;; this is a plain old value, not a promise. decread the
                      ;; num-bindings to take account
                      (decf ,num-bindings))
                  (when (<= ,num-bindings (hash-table-count track-promise-fired))
                    (let ((vars (loop for bind in ',bind-vars collect (getf ,finished-vals bind))))
                      (apply #'finish (append (list ,finished-promise) vars))))))))
       ;; for each binding, attach a callback to the promise it generates that
       ;; marks itself as complete. once all binding forms report in, the main
       ;; promise "finished-promise" is triggered, which runs the body
       ,@(loop for (bind form) in bindings collect
           `(let ((,promise (promisify ,form)))
              ;; when this promise finishes, call the finished-cb, which tallies
              ;; up the number of finishes until it equals the number of
              ;; bindings.
              (attach-errback ,promise
                (lambda (e)
                  (signal-error ,finished-promise e)))
              (attach ,promise
                (lambda (&rest ,args)
                  (setf (getf ,finished-vals ',bind) (car ,args))
                  (funcall ,finished-cb ,promise)))))
       ;; return our promise which gets fired when all bindings have completed.
       ;; gets events forwarded to it from the binding promises.
       (attach ,finished-promise
         (lambda ,bind-vars
           ,@(progn
               (when ignore-bindings
                 (push `(declare (ignore ,@ignore-bindings)) body))
               body))))))

(defmacro alet* (bindings &body body)
  "Asynchronous let*. Allows calculating a number of values in sequence via
   promises, and run the body when all values have computed with the bindings
   given available to the body.

   Also returns a promise that fires with the values returned from the body form,
   which allows arbitrary nesting to get a final value(s)."
  (let* ((ignore-bindings nil)
         ;; any nil bindings are replaces with an ignored gensym symbol, which
         ;; is also added to the ignore-bindings list
         (bindings (loop for (bind form) in bindings
                         collect (if bind
                                     (list bind form)
                                     (let ((ignore-sym (gensym "ignore")))
                                       (push ignore-sym ignore-bindings)
                                       (list ignore-sym form)))))
         ;; wrap body in let form which allows (declare ...)
         (body-form body))
    ;; loop over bindings in reverse and build a nested list into the body-form
    ;; variable
    (dolist (binding (reverse bindings))
      (let ((bind (car binding))
            (promise (cadr binding))
            (args (gensym "args")))
        (setf body-form
              `((attach ,promise
                  (lambda (&rest ,args)
                    (let ((,bind (car ,args)))
                      ,@(progn
                          (when (member bind ignore-bindings)
                            (push `(declare (ignore ,bind)) body-form))
                          body-form))))))))
    (car body-form)))

(defmacro aif (promise-gen true-form false-form)
  "Acts like `if` except that the evaluated form accepts a promise:
     (aif (async-action)
          (it-worked!)
          (nope-sad-face))"
  (let ((bind (gensym "aif-res")))
    `(alet ((,bind ,promise-gen))
       (if ,bind
           ,true-form
           ,false-form))))

(defmacro multiple-promise-bind ((&rest bindings) promise-gen &body body)
  "Like multiple-value-bind, but instead of a form that evaluates to multiple
   values, takes a form that generates a promise."
  (let* ((args (gensym "args"))
         (ignore-bindings nil)
         ;; replace nil bindings with symbols that will be explicitely ignored
         (bindings (loop for binding in bindings
                         collect (if (null binding)
                                     (let ((ignored (gensym "ignored-binding")))
                                       (push ignored ignore-bindings)
                                       ignored)
                                     binding))))
    `(attach ,promise-gen
       (lambda (&rest ,args)
         (let (,@bindings)
           ;; ignore any nil bindings
           ,(when ignore-bindings
              `(declare (ignore ,@ignore-bindings)))
           ;; set the values into our bindings
           ,@(loop for b in bindings collect
               (if (member b ignore-bindings)
                   `(setf ,args (cdr ,args))
                   `(setf ,b (car ,args)
                          ,args (cdr ,args))))
           ;; wrap in another let in case users want to add their own declare
           (let (,@(loop for b in bindings
                         unless (member b ignore-bindings)
                         collect (list b b)))
             ,@body))))))

(defmacro wait (promise-gen &body body)
  "Wait for a promise to finish, ignoring any values it returns. Can be useful
   when you want to run an async action but don't care about the return value
   (or it doesn't return a value) and you want to continue processing when it
   returns."
  (let ((ignore-var (gensym "async-ignore")))
    `(attach ,promise-gen
       (lambda (&rest ,ignore-var)
         (declare (ignore ,ignore-var))
         ,@body))))

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
                    (promise-handler-case
                      ,(if promise-bind
                           `(let ((,promise-bind (make-promise)))
                              (wait-for ,promise-bind (,next-fn))
                              ,@body)
                           `(wait-for (progn ,@body) (,next-fn)))
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
  (with-promise (resolve reject)
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

