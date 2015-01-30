(in-package :blackbird-syntax)

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
         (declare (ignorable ,args))
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

(defmacro walk (&body body)
  "Treat each operation in body as a sequential, promise-returning operation and
   then resolve the returned promise with the value(s) of the last operation in
   body."
  (let (last)
    `(alet* ,(loop for (head . tail) on body
                  when tail
                  collect `(nil ,head)
                  else do (setf last head))
       ,last)))

(defmacro walk1 (save-values-form &body body)
  "Like walk, except returns the value(s) of the first form instead of the last."
  (let ((tmp-vals (gensym "tmp-vals")))
    `(attach ,save-values-form
       (lambda (&rest ,tmp-vals)
         (alet* ,(loop for form in body collect `(nil ,form))
           (apply 'values ,tmp-vals))))))

