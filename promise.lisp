(in-package :blackbird-base)

(defvar *debug-on-error* nil
  "If t, will not catch errors passing through the handlers and will let them
   bubble up to the debugger.")

(defvar *promise-keep-specials* '()
  "Names of special variables to be preserved during promise callbacks")

(defvar *promise-finish-hook* (lambda (finish-fn) (funcall finish-fn))
  "This is a function of one argument: a function of 0 args that is called to
   finish a promise. By default it just finishes the promise, but can be
   replaced to add a delay to the promise or finish it from another thread.")

(defclass promise ()
  ((name :accessor promise-name :initarg :name :initform nil
    :documentation "Lets a promise be named this is good for debugging.")
   (callbacks :accessor promise-callbacks :initform nil
    :documentation "A list that holds all callbacks associated with this promise.")
   (errbacks :accessor promise-errbacks :initform nil
    :documentation "A list that holds all errbacks associated with this promise.")
   (forwarded-promise :accessor promise-forward-to :initform nil
    :documentation "Can hold a reference to another promise, which will receive
                    callbacks and event handlers added to this one once set.
                    This allows a promise to effectively take over another promise
                    by taking all its callbacks/events.")
   (finished :accessor promise-finished :reader promise-finished-p :initform nil
    :documentation "Marks if a promise has been finished or not.")
   (errored :accessor promise-errored :reader promise-errored-p :initform nil
    :documentation "Marks if an error occured on this promise.")
   (error :accessor promise-error :initform nil
    :documentation "Holds an error value for this promise.")
   (values :accessor promise-values :initform nil
    :documentation "Holds the finished value(s) of the promise."))
  (:documentation
    "Defines a class which represents a value that MAY be ready sometime in the
     future. Also supports attaching callbacks to the promise such that they will
     be called with the computed value(s) when ready."))

(defun wrap-callback (callback)
  (let ((all-vars *promise-keep-specials*)) ; avoid unneeded thread-unsafety
    (if (null all-vars)
        callback
        (let* ((bound (remove-if-not #'boundp all-vars))
               (vars (append bound (remove-if #'boundp all-vars)))
               (vals (mapcar #'symbol-value bound)))
          #'(lambda (&rest args)
              (let ((*promise-keep-specials* all-vars))
                (progv vars vals (apply callback args))))))))

(defmacro with-error-handling ((blockname &optional promise) error-fn &body body)
  "Wraps some nice restarts around the bits of code that run our promises and
   handles errors."
  (let ((last-err (gensym "last-err")))
    `(let ((,last-err nil))
       (block ,blockname
         (handler-bind
             ((error (lambda (e)
                       (setf ,last-err e)
                       (unless *debug-on-error*
                         (funcall ,error-fn e)))))
           (restart-case
             (progn ,@body)
             (reject-promise ()
               :report (lambda (s) (format s "Reject the promise ~a" ,promise))
               (format *debug-io* "~&;; promise rejected~%")
               (funcall ,error-fn ,last-err))))))))

(defmethod print-object ((promise promise) s)
  (print-unreadable-object (promise s :type t :identity t)
    (when (promise-name promise)
      (format s "~_name: ~s " (promise-name promise)))
    ;(format s "~_callback(s): ~s " (length (promise-callbacks promise)))
    ;(format s "~_errback(s): ~s " (length (promise-errbacks promise)))
    (format s "~_finished: ~a " (promise-finished-p promise))
    (format s "~_errored: ~a " (promise-errored-p promise))
    (format s "~_forward: ~a" (not (not (promise-forward-to promise))))))

(defun make-promise (&key name)
  "Create a blank promise."
  (make-instance 'promise :name name))

(defun create-promise (create-fn &key name)
  "Returns a new promise, which can be finished/signaled via the given create-fn
   function, which takes exactly two values: a resolve function which is called
   with an arbitrary number of arguments and finishes the promise, and a reject
   function which takes a condition object and signals the condition on the
   promise."
  (let* ((promise (make-promise :name name))
         (resolve-fn (lambda (&rest vals) (apply 'finish (append (list promise) vals))))
         (reject-fn (lambda (condition) (signal-error promise condition))))
    (with-error-handling (errexit promise)
      (lambda (e)
        (funcall reject-fn e)
        (return-from errexit))
      (funcall create-fn resolve-fn reject-fn))
    (vom:debug "create-promise: ~a" promise)
    promise))

(defmacro with-promise ((resolve reject
                         &key (resolve-fn (gensym "resolve-fn"))
                              (reject-fn (gensym "reject-fn"))
                              name)
                         &body body)
  "Wraps create-promise in nicer syntax:

     (with-promise (resolve reject)
       (do-something (lambda (result)
                       (resolve result))))
       => promise"
  `(create-promise
     (lambda (,resolve-fn ,reject-fn)
       (declare (ignorable ,resolve-fn ,reject-fn))
       (macrolet ((,resolve (&rest args)
                    (if (= 1 (length args))
                        `(apply ,',resolve-fn (multiple-value-list ,(car args)))
                        `(funcall ,',resolve-fn ,@args))))
         (flet ((,reject (condition) (funcall ,reject-fn condition)))
           (declare (ignorable #',reject))
           ,@body)))
     :name ,name))

(defun do-promisify (fn &key name)
  "Turns any value or set of values into a promise, unless a promise is passed
   in which case it is returned."
  (let ((promise (make-promise :name name)))
    (with-error-handling (errexit promise)
      (lambda (e)
        (signal-error promise e)
        (return-from errexit))
      (let* ((vals (multiple-value-list (funcall fn)))
             (new-promise (car vals)))
        (if (promisep new-promise)
            (setf promise new-promise)
            (apply 'finish promise vals))))
    (vom:debug "promisify: ~a" promise)
    promise))

(defmacro promisify (promise-gen)
  "Turns any value or set of values into a promise, unless a promise is passed
   in which case it is returned."
  `(do-promisify (lambda () ,promise-gen) :name ,(format nil "promisify: ~s" promise-gen)))

(defun promisep (promise)
  "Is this a promise?"
  (subtypep (type-of promise) 'promise))

(defun do-add-callback (promise cb)
  "Add a callback to a promise."
  (push cb (promise-callbacks promise)))

(defun do-attach-errback (promise errback)
  "Add an error handler for this promise."
  (let ((new-promise (make-promise)))
    (if (promisep promise)
        ;; return a promise that's fired with the return value of the error
        ;; handler
        (let ((forwarded-promise (lookup-forwarded-promise promise))
              (wrapped (if (consp errback)
                           errback
                           (cons new-promise errback))))
          (push wrapped (promise-errbacks forwarded-promise))
          (run-promise forwarded-promise))
        ;; pass along the given value
        (finish new-promise promise))
    new-promise))

(defun attach-errback (promise errback)
  "Add an error handler for this promise."
  (do-attach-errback promise (wrap-callback errback)))

(defun setup-promise-forward (promise-from promise-to)
  "Set up promise-from to send all callbacks, events, handlers, etc to the
   promise-to promise. This includes all current objects, plus objects that may be
   added later on. For instance, if you forward promise A to promise B, adding an
   event handler to promise A will then add it to promise B (assuming promise B has
   no current event handler). The same goes for callbacks as well, they will be
   added to the new promise-to if added to the promise-from."
  ;; a promise "returned" another promise. reattach the callbacks/errbacks from
  ;; the original promise onto the returned one
  (setf promise-to (lookup-forwarded-promise promise-to))
  (dolist (cb (reverse (promise-callbacks promise-from)))
    (do-add-callback promise-to cb))
  (dolist (errback (reverse (promise-errbacks promise-from)))
    (do-attach-errback promise-to errback))
  ;; mark the promise as forwarded so other parts of the system know to use the
  ;; new promise for various tasks.
  (setf (promise-forward-to promise-from) promise-to))

(defun lookup-forwarded-promise (promise)
  "This function follows forwarded promises until it finds the last in the chain
   of forwarding."
  (when (promisep promise)
    (loop while (promise-forward-to promise) do
      (setf promise (promise-forward-to promise))))
  promise)

(defun run-promise (promise)
  "Run all errorbacks if an error occured on the promise, or all callbacks if
   the promise is finished. If neither of those conditions are met, nothing
   happens."
  (if (promise-errored-p promise)
      (when (promise-errbacks promise)
        (let ((errbacks (reverse (promise-errbacks promise)))
              (error (promise-error promise)))
          (setf (promise-errbacks promise) nil)
          (dolist (errback-entry errbacks)
            (let* ((promise (car errback-entry))
                   (errback (cdr errback-entry))
                   (res (funcall errback error)))
              (finish promise res)))))
      (when (promise-finished-p promise)
        (let ((callbacks (promise-callbacks promise))
              (values (promise-values promise)))
          (setf (promise-callbacks promise) nil)
          (dolist (cb (reverse callbacks))
            (apply cb values)))))
  promise)

(defun finish (promise &rest values)
  "Mark a promise as finished, along with all values it's finished with. If
   finished with another promise, forward the current promise to the new one."
  (if (or (promise-finished-p promise)
          (promise-errored-p promise)
          (promise-forward-to promise))
      (vom:debug "resolving an already-resolved promise: ~a" promise)
      (let ((new-promise (car values)))
        (funcall *promise-finish-hook*
          (lambda ()
            (vom:debug "finish: ~a ~s" promise values)
            (cond ((promisep new-promise)
                   ;; set up the current promise to forward all callbacks/handlers/events
                   ;; to the new promise from now on.
                   (setup-promise-forward promise new-promise)
                   ;; run the new promise
                   (run-promise new-promise))
                  (t
                   ;; just a normal finish, run the promise
                   (setf (promise-finished promise) t
                         (promise-values promise) values)
                   (run-promise promise)))))
        promise)))

(defun signal-error (promise condition)
  "Signal that an error has happened on a promise. If the promise has errbacks,
   they will be used to process the error, otherwise it will be stored until an
   errback is added to the promise."
  (if (or (promise-errored-p promise)
          (promise-finished-p promise)
          (promise-forward-to promise))
      (vom:debug "rejecting an already-resolved promise: ~a" promise)
      (when (promisep promise)
        (vom:debug "signal-error: ~a / ~a" promise condition)
        (let ((forwarded-promise (lookup-forwarded-promise promise)))
          (setf (promise-error forwarded-promise) condition)
          (setf (promise-errored forwarded-promise) t)
          (run-promise forwarded-promise)))))

(defun reset-promise (promise)
  "Clear out all callbacks/errbacks. Useful for halting a promise's execution."
  (let ((promise (lookup-forwarded-promise promise)))
    (setf (promise-callbacks promise) nil
          (promise-errbacks promise) nil
          (promise-error promise) nil
          (promise-values promise) nil
          (promise-finished promise) nil))
  promise)

(defun do-attach (promise cb &key name)
  "Attach a callback to a promise. The promise must be the first value in a list
   of values (car promise-values) OR the promise-values will be apply'ed to cb."
  (let* ((promise (lookup-forwarded-promise promise))
         (cb-return-promise (make-promise :name name))
         (cb-wrapped (lambda (&rest args)
                       (with-error-handling (errexit promise)
                         (lambda (e)
                           (signal-error cb-return-promise e)
                           (return-from errexit))
                         (let ((cb-return (multiple-value-list (apply cb args))))
                           (apply #'finish (append (list cb-return-promise)
                                                   cb-return)))))))
    (attach-errback promise
      (lambda (e) (signal-error cb-return-promise e)))
    (do-add-callback promise (wrap-callback cb-wrapped))
    (run-promise promise)
    cb-return-promise))

(defmacro attach (promise-gen cb)
  "Macro wrapping attachment of callback to a promise (takes multiple values into
   account, which a simple function cannot)."
  `(do-attach (promisify ,promise-gen) ,cb :name ,(format nil "attach: ~s" promise-gen)))

(defun do-catch (promise handler-fn)
  "Catch errors in the promise chain and run a handler function when caught."
  (with-promise (resolve reject :resolve-fn resolve-fn)
    (attach-errback promise
      (lambda (e)
        (with-error-handling (errexit)
          (lambda (e)
            (return-from errexit (reject e)))
          (resolve (funcall handler-fn e)))))
    (attach promise resolve-fn)))

(defmacro catcher (promise-gen &rest handler-forms)
  "Catch errors in the promise chain and run a handler function when caught."
  `(do-catch (promisify ,promise-gen)
     (lambda (e)
       (typecase e
         ,@(loop for x in handler-forms collect
             (list (car x)
                   (let ((bind (caadr x)))
                     (if bind
                         `(let ((,(caadr x) e)) ,@(cddr x))
                         `(progn ,@(cddr x))))))))))

(defun do-tap (promise tap-fn)
  "Gives a handler function access to a promise's value but finishes the
   returned with the values of the given promise (instead of the return values
   of the given function). This allows installing a read-only hook into the
   promise chain, useful for logging or other such activities."
  (with-promise (resolve reject :resolve-fn resolver)
    (attach-errback promise (lambda (err) (reject err)))
    (attach promise
      (lambda (&rest vals)
        (attach (apply tap-fn vals)
          (lambda (&rest args)
            (declare (ignore args))
            (apply resolver vals)))))))

(defmacro tap (promise-gen tap-fn)
  "Gives a handler function access to a promise's value but finishes the
   returned with the values of the given promise (instead of the return values
   of the given function). This allows installing a read-only hook into the
   promise chain, useful for logging or other such activities."
  `(do-tap (promisify ,promise-gen) ,tap-fn))

(defun do-finally (promise finally-fn)
  "Run the finally-fn whether the given promise has a value or an error."
  (with-promise (resolve reject :resolve-fn resolver)
    (attach-errback promise
      (lambda (err)
        (attach (funcall finally-fn)
          (lambda (&rest _)
            (declare (ignore _))
            (reject err)))))
    (attach promise
      (lambda (&rest args)
        (attach (funcall finally-fn)
          (lambda (&rest _)
            (declare (ignore _))
            (apply resolver args)))))))


(defmacro finally (promise-gen &body body)
  "Run the body form whether the given promise has a value or an error."
  `(do-finally (promisify ,promise-gen) (lambda () ,@body)))

