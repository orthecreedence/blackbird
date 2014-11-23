(in-package :blackbird)

(defvar *promise-keep-specials* '()
  "Names of special variables to be preserved during promise callbacks")

(defclass promise ()
  ((callbacks :accessor promise-callbacks :initform nil
    :documentation "A list that holds all callbacks associated with this promise.")
   (errbacks :accessor promise-errbacks :initform nil
    :documentation "A list that holds all errbacks associated with this promise.")
   (forwarded-promise :accessor promise-forward-to :initform nil
    :documentation "Can hold a reference to another promise, which will receive
                    callbacks and event handlers added to this one once set.
                    This allows a promise to effectively take over another promise
                    by taking all its callbacks/events.")
   (preserve-callbacks :accessor promise-preserve-callbacks :initarg :preserve-callbacks :initform nil
    :documentation "When nil (the default) detaches callbacks after running
                    promise.")
   (reattach-callbacks :accessor promise-reattach-callbacks :initarg :reattach-callbacks :initform t
    :documentation "When a promise's callback returns another promise, bind all
                    callbacks from this promise onto the returned one. Allows
                    values to transparently be derived from many layers deep of
                    promises, almost like a real call stack.")
   (finished :accessor promise-finished :reader promise-finished-p :initform nil
    :documentation "Marks if a promise has been finished or not.")
   (events :accessor promise-events :initform nil
    :documentation "Holds events for this promise, to be handled with event-handler.")
   (values :accessor promise-values :initform nil
    :documentation "Holds the finished value(s) of the computer promise. Will be
                    apply'ed to the callbacks."))
  (:documentation
    "Defines a class which represents a value that MAY be ready sometime in the
     promise. Also supports attaching callbacks to the promise such that they will
     be called with the computed value(s) when ready."))

(defun wrap-callback (callback)
  (let ((all-vars *promise-keep-specials*)) ; avoid unneeded thread-unsafety
    (if (null all-vars)
        callback
        (let* ((bound (remove-if-not #'boundp all-vars))
               (vars (append bound (remove-if #'boundp all-vars)))
               (vals (mapcar #'symbol-value bound)))
          #'(lambda (&rest args)
              (progv vars vals (apply callback args)))))))

(defmethod print-object ((promise promise) s)
  (print-unreadable-object (promise s :type t :identity t)
    (format s "~_callback(s): ~s " (length (promise-callbacks promise)))
    (format s "~_errback(s): ~s " (length (promise-errbacks promise)))
    (format s "~_finished: ~a " (promise-finished promise))
    (format s "~_forward: ~a" (not (not (promise-forward-to promise))))))

(defun make-promise (&key preserve-callbacks (reattach-callbacks t))
  "Create a blank promise."
  (make-instance 'promise :preserve-callbacks preserve-callbacks
                         :reattach-callbacks reattach-callbacks))

(defun promisep (promise)
  "Is this a promise?"
  (subtypep (type-of promise) 'promise))

(defun do-add-callback (promise cb)
  "Add a callback to a promise if it isn't already attached."
  (unless (member cb (promise-callbacks promise))
    (push cb (promise-callbacks promise))))

(defun do-attach-errback (promise errback)
  "Add an error handler for this promise. If the errback already exists on this
   promise, don't re-add it."
  (when (promisep promise)
    (let ((forwarded-promise (lookup-forwarded-promise promise)))
      (unless (member errback (promise-errbacks forwarded-promise))
        (push errback (promise-errbacks forwarded-promise))
        (process-errors forwarded-promise))))
  promise)

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
  (dolist (cb (promise-callbacks promise-from))
    (do-add-callback promise-to cb))
  (dolist (errback (promise-errbacks promise-from))
    (do-attach-errback promise-to errback))
  ;; mark the promise as forwarded to other parts of the system know to use the
  ;; new promise for various tasks.
  (setf (promise-forward-to promise-from) promise-to))

(defun lookup-forwarded-promise (promise)
  "This function follows forwarded promises until it finds the last in the chain
   of forwarding."
  (when (promisep promise)
    (loop while (promise-forward-to promise) do
      (setf promise (promise-forward-to promise))))
  promise)

(defun process-errors (promise)
  "If an event handler exists for this promise, run all events through the
   errbacks and clear the events out once run."
  (when (promise-errbacks promise)
    (dolist (ev (reverse (promise-events promise)))
      (dolist (errback (reverse (promise-errbacks promise)))
        (funcall errback ev)))
    (setf (promise-events promise) nil)))

(defun signal-error (promise condition)
  "Signal that an error has happened on a promise. If the promise has errbacks,
   they will be used to process the error, otherwise it will be stored until an
   errback is added to the promise."
  ;; if we're debugging just throw the error
  (when (find :promise-debug *features*)
    (error condition))
  (let ((forwarded-promise (lookup-forwarded-promise promise)))
    (push condition (promise-events forwarded-promise))
    (process-errors forwarded-promise)))

(defun run-promise (promise)
  "Run all callbacks on a promise *IF* the promise is finished (and has computed
   values). If preserve-callbacks in the promise is set to nil, the promise's
   callbacks will be detached after running."
  (when (promise-finished promise)
    (let ((callbacks (promise-callbacks promise))
          (values (promise-values promise)))
      (dolist (cb (reverse callbacks))
        (apply cb values)))
    ;; clear out the callbacks if specified
    (unless (promise-preserve-callbacks promise)
      (setf (promise-callbacks promise) nil))
    promise))

(defun finish (promise &rest values)
  "Mark a promise as finished, along with all values it's finished with. If
   finished with another promise, forward the current promise to the new one."
  (let ((new-promise (car values)))
    (cond ((and (promisep new-promise)
                (promise-reattach-callbacks promise))
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

(defun reset-promise (promise)
  "Clear out all callbacks/errbacks. Useful for halting a promise's execution."
  (let ((promise (lookup-forwarded-promise promise)))
    (setf (promise-callbacks promise) nil
          (promise-errbacks promise) nil
          (promise-events promise) nil
          (promise-values promise) nil
          (promise-finished promise) nil))
  promise)

(defun attach-cb (promise-values cb)
  "Attach a callback to a promise. The promise must be the first value in a list
   of values (car promise-values) OR the promise-values will be apply'ed to cb."
  (let* ((promise promise-values)
         (promise (if (promisep promise)
                     promise
                     (car promise-values)))
         (promise (if (promisep promise)
                     (lookup-forwarded-promise promise)  ; follow forwarded promises
                     promise))
         (cb-return-promise (make-promise))
         (cb-wrapped (lambda (&rest args)
                       (let ((cb-return (multiple-value-list (apply cb args))))
                         (apply #'finish (append (list cb-return-promise)
                                                 cb-return))))))
    ;; if we were indeed passed a promise, attach the callback to it AND run the
    ;; promise if it has finished.
    (if (promisep promise)
        (progn
          (do-add-callback promise (wrap-callback cb-wrapped))
          (run-promise promise))
        ;; not a promise, just a value. run the callback directly
        (apply cb-wrapped promise-values))
    cb-return-promise))

(defmacro attach (promise-gen cb)
  "Macro wrapping attachment of callback to a promise (takes multiple values into
   account, which a simple function cannot)."
  (let ((promise-values (gensym "promise-values")))
    `(let ((,promise-values (multiple-value-list ,promise-gen)))
       (attach-cb ,promise-values ,cb))))

;; -----------------------------------------------------------------------------
;; start our syntactic abstraction section (rolls off the tongue nicely)
;; -----------------------------------------------------------------------------

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
           `(let ((promise-gen (multiple-value-list ,form)))
              ;; when this promise finishes, call the finished-cb, which tallies
              ;; up the number of finishes until it equals the number of
              ;; bindings.
              (attach (apply #'values promise-gen)
                (lambda (&rest ,args)
                  (setf (getf ,finished-vals ',bind) (car ,args))
                  (funcall ,finished-cb (car promise-gen))))))
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
           (,promise-sym (make-promise)))
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

(defmacro %handler-case (body &rest bindings)
  "Simple wrapper around handler-case that allows switching out the form to make
   macroexpansion a lot easier to deal with."
  `(handler-case ,body ,@bindings))

(defmacro wrap-event-handler (promise-gen error-forms)
  "Used to wrap the promise-generation forms of promise syntax macros. This macro
   is not to be used directly, but instead by promise-handler-case.

   It allows itself to be recursive, but any recursions will simply add their
   error forms for a top-level list and return the form they are given as the
   body. This allows a top-level form to add an error handler to a promise, while
   gathering the lower-level forms' handler-case bindings into one big handler
   function (created with make-nexted-handler-cases).

   Note that since normally the wrap-event-handler forms expand outside in, we
   have to do some trickery with the error-handling functions to make sure the
   order of the handler-case forms (as far as what level of the tree we're on)
   are preserved."
  (let ((signal-error (gensym "signal-error"))
        (handler-fn (gensym "handler-fn"))
        (vals (gensym "vals")))
    ;; hijack any child wrap-event-handler macros to just return their
    ;; promise-gen form verbatim, but add their error handlers to the error
    ;; handling chain
    `(macrolet ((wrap-event-handler (promise-gen error-forms)
                  (let ((old-signal-error (gensym "old-signal-error")))
                    `(progn
                       ;; "inject" the next-level down error handler in between the
                       ;; error triggering function and the error handler one level
                       ;; up. this preserves the handler-case tree (as opposed to
                       ;; reversing it)
                       ;; NOTE that signal-error is defined *below* in the body
                       ;; of the macrolet form
                       (let ((,old-signal-error ,',signal-error))
                         (setf ,',signal-error
                               (lambda (ev)
                                 (%handler-case
                                   (funcall ,old-signal-error ev)
                                   ,@error-forms))))
                       ;; return the promise-gen form verbatim
                       ,promise-gen))))
       ;; define a function that signals the error, and a top-level error handler
       ;; which uses the error-forms passed to THIS macro instance. any instance
       ;; of `wrap-event-handler` that occurs in the `promise-gen` form will inject
       ;; its error handler between handler-fn and signal-error.
       (let* ((,signal-error (lambda (ev) (error ev)))
              (,handler-fn (lambda (ev)
                             (%handler-case
                               (funcall ,signal-error ev)
                               ,@error-forms)))
              ;; sub (wrap-event-handler ...) forms are expanded with ,promise-gen
              ;; they add their handler-case forms into a lambda which is injected
              ;; into the error handling chain,
              (,vals (multiple-value-list ,promise-gen)))
         (if (promisep (car ,vals))
             (progn
               (attach-errback (car ,vals) ,handler-fn))
             (apply #'values ,vals))))))

(defmacro promise-handler-case (body-form &rest error-forms &environment env)
  "Wrap all of our lovely attach macro up with an event handler. This is more or
   less restricted to the form it's run in.

   Note that we only have to wrap (attach) because *all other syntax macros* use
   attach. This greatly simplifies our code.

   Note that if we just wrap `attach` directly in a macrolet, it expands
   infinitely (probably no what we want). So we're doing some trickery here. We
   use the environment from the top-level macro to grab the original macro
   function and make it available from *within* the macrolet. This allows
   the macrolet to redefine the `attach` macro while also simultaneously
   expanding the previous definition of it. This allows wrapped calls of
   promise-handler-case to add layers of error handling around any `attach` call
   that is within lexical grasp."
  (if (or (find :promise-debug *features*)
          (find :future-debug *features*))
      ;; we're debugging promises...disable all error handling (so errors bubble
      ;; up to main loop)
      body-form
      ;; wrap the top-level form in a handler-case to catch any errors we may
      ;; have before the promises are even generated.
      `(%handler-case
         ;; redefine our attach macro so that the promise-gen forms are
         ;; wrapped (recursively, if called more than once) in the
         ;; `wrap-event-handler` macro.
         (macrolet ((attach (promise-gen fn &environment ml-env)
                      (let ((args (gensym "phc-wrap-args")))
                        ;; call the original attach macro (via our pass env).
                        ;; this allows calling it without throwing macrolet
                        ;; into an endless loop
                        (funcall (macro-function 'attach ',env)
                          `(attach
                             (wrap-event-handler ,promise-gen ,',error-forms)
                             ;; create a wrapper function around the given
                             ;; callback that applies our error handlers
                             (lambda (&rest ,args)
                               (%handler-case
                                 (apply ,fn ,args)
                                 ,@',error-forms)))
                          ml-env))))
             ,body-form)
         ,@error-forms)))
