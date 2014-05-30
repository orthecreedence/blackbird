(in-package :cl-async-future)

(defclass future ()
  ((callbacks :accessor future-callbacks :initform nil
    :documentation "A list that holds all callbacks associated with this future.")
   (errbacks :accessor future-errbacks :initform nil
    :documentation "A list that holds all errbacks associated with this future.")
   (forwarded-future :accessor future-forward-to :initform nil
    :documentation "Can hold a reference to another future, which will receive
                    callbacks and event handlers added to this one once set.
                    This allows a future to effectively take over another future
                    by taking all its callbacks/events.")
   (preserve-callbacks :accessor future-preserve-callbacks :initarg :preserve-callbacks :initform nil
    :documentation "When nil (the default) detaches callbacks after running
                    future.")
   (reattach-callbacks :accessor future-reattach-callbacks :initarg :reattach-callbacks :initform t
    :documentation "When a future's callback returns another future, bind all
                    callbacks from this future onto the returned one. Allows
                    values to transparently be derived from many layers deep of
                    futures, almost like a real call stack.")
   (finished :accessor future-finished :reader future-finished-p :initform nil
    :documentation "Marks if a future has been finished or not.")
   (events :accessor future-events :initform nil
    :documentation "Holds events for this future, to be handled with event-handler.")
   (values :accessor future-values :initform nil
    :documentation "Holds the finished value(s) of the computer future. Will be
                    apply'ed to the callbacks."))
  (:documentation
    "Defines a class which represents a value that MAY be ready sometime in the
     future. Also supports attaching callbacks to the future such that they will
     be called with the computed value(s) when ready."))

(defmethod print-object ((future future) s)
  (print-unreadable-object (future s :type t :identity t)
    (format s "~_callback(s): ~s " (length (future-callbacks future)))
    (format s "~_errback(s): ~s " (length (future-errbacks future)))
    (format s "~_finished: ~a " (future-finished future))
    (format s "~_forward: ~a" (not (not (future-forward-to future))))))

(defun make-future (&key preserve-callbacks (reattach-callbacks t))
  "Create a blank future."
  (make-instance 'future :preserve-callbacks preserve-callbacks
                         :reattach-callbacks reattach-callbacks))

(defun futurep (future)
  "Is this a future?"
  (subtypep (type-of future) 'future))

(defun do-add-callback (future cb)
  "Add a callback to a future if it isn't already attached."
  (unless (member cb (future-callbacks future))
    (push cb (future-callbacks future))))

(defun attach-errback (future errback)
  "Add an error handler for this future. If the errback already exists on this
   future, don't re-add it."
  (when (futurep future)
    (let ((forwarded-future (lookup-forwarded-future future)))
      (unless (member errback (future-errbacks forwarded-future))
        (push errback (future-errbacks forwarded-future))
        (process-errors forwarded-future))))
  future)

(defun setup-future-forward (future-from future-to)
  "Set up future-from to send all callbacks, events, handlers, etc to the
   future-to future. This includes all current objects, plus objects that may be
   added later on. For instance, if you forward future A to future B, adding an
   event handler to future A will then add it to future B (assuming future B has
   no current event handler). The same goes for callbacks as well, they will be
   added to the new future-to if added to the future-from."
  ;; a future "returned" another future. reattach the callbacks/errbacks from
  ;; the original future onto the returned one
  (dolist (cb (future-callbacks future-from))
    (do-add-callback future-to cb))
  (dolist (errback (future-errbacks future-from))
    (attach-errback future-to errback))
  ;; mark the future as forwarded to other parts of the system know to use the
  ;; new future for various tasks.
  (setf (future-forward-to future-from) future-to))

(defun lookup-forwarded-future (future)
  "This function follows forwarded futures until it finds the last in the chain
   of forwarding."
  (when (futurep future)
    (loop while (future-forward-to future) do
      (setf future (future-forward-to future))))
  future)

(defun process-errors (future)
  "If an event handler exists for this future, run all events through the
   errbacks and clear the events out once run."
  (when (future-errbacks future)
    (dolist (ev (reverse (future-events future)))
      (dolist (errback (reverse (future-errbacks future)))
        (funcall errback ev)))
    (setf (future-events future) nil)))

(defun signal-error (future condition)
  "Signal that an error has happened on a future. If the future has errbacks,
   they will be used to process the error, otherwise it will be stored until an
   errback is added to the future."
  ;; if we're debugging just throw the error
  (when (find :future-debug *features*)
    (error condition))
  (let ((forwarded-future (lookup-forwarded-future future)))
    (push condition (future-events forwarded-future))
    (process-errors forwarded-future)))

(defun run-future (future)
  "Run all callbacks on a future *IF* the future is finished (and has computed
   values). If preserve-callbacks in the future is set to nil, the future's
   callbacks will be detached after running."
  (when (future-finished future)
    (let ((callbacks (future-callbacks future))
          (values (future-values future)))
      (dolist (cb (reverse callbacks))
        (apply cb values)))
    ;; clear out the callbacks if specified
    (unless (future-preserve-callbacks future)
      (setf (future-callbacks future) nil))
    future))

(defun finish (future &rest values)
  "Mark a future as finished, along with all values it's finished with. If
   finished with another future, forward the current future to the new one."
  (let ((new-future (car values)))
    (cond ((and (futurep new-future)
                (future-reattach-callbacks future))
           ;; set up the current future to forward all callbacks/handlers/events
           ;; to the new future from now on.
           (setup-future-forward future new-future)
           ;; run the new future
           (run-future new-future))
          (t
           ;; just a normal finish, run the future
           (setf (future-finished future) t
                 (future-values future) values)
           (run-future future)))))

(defun reset-future (future)
  "Clear out all callbacks/errbacks. Useful for halting a future's execution."
  (let ((future (lookup-forwarded-future future)))
    (setf (future-callbacks future) nil
          (future-errbacks future) nil
          (future-events future) nil
          (future-values future) nil
          (future-finished future) nil))
  future)

(defun attach-cb (future-values cb)
  "Attach a callback to a future. The future must be the first value in a list
   of values (car future-values) OR the future-values will be apply'ed to cb."
  (let* ((future future-values)
         (future (if (futurep future)
                     future
                     (car future-values)))
         (future (if (futurep future)
                     (lookup-forwarded-future future)  ; follow forwarded futures
                     future))
         (cb-return-future (make-future))
         (cb-wrapped (lambda (&rest args)
                       (let ((cb-return (multiple-value-list (apply cb args))))
                         (apply #'finish (append (list cb-return-future)
                                                 cb-return))))))
    ;; if we were indeed passed a future, attach the callback to it AND run the
    ;; future if it has finished.
    (if (futurep future)
        (progn
          (do-add-callback future cb-wrapped)
          (run-future future))
        ;; not a future, just a value. run the callback directly
        (apply cb-wrapped future-values))
    cb-return-future))

(defmacro attach (future-gen cb)
  "Macro wrapping attachment of callback to a future (takes multiple values into
   account, which a simple function cannot)."
  (let ((future-values (gensym "future-values")))
    `(let ((,future-values (multiple-value-list ,future-gen)))
       (attach-cb ,future-values ,cb))))

;; -----------------------------------------------------------------------------
;; start our syntactic abstraction section (rolls off the tongue nicely)
;; -----------------------------------------------------------------------------

(defmacro alet (bindings &body body)
  "Asynchronous let. Allows calculating a number of values in parallel via
   futures, and runs the body when all values have computed with the bindings
   given available to the body.
   
   Also returns a future that fires with the values returned from the body form,
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
         (finished-future (gensym "finished-future"))
         (finished-vals (gensym "finished-vals"))
         (finished-cb (gensym "finished-cb"))
         (inc-value (gensym "inc-value"))
         (args (gensym "args")))
    `(let* ((,num-bindings ,(length bindings)) ; make a local var for num-bindings
            (,finished-future (make-future))
            (,finished-vals nil)
            (,finished-cb
              ;; the hash table makes sure that *all* futures have fires at
              ;; least once. used to be a simple counter, but that doesn't
              ;; account for a future firiing multiple times before its alet
              ;; brethren get a chance to finish.
              (let ((track-future-fired (make-hash-table :test #'eq :size ,num-bindings)))
                (lambda (future)
                  (if (futurep future)
                      ;; mark this future as finished in the tracking has
                      (setf (gethash future track-future-fired) t)
                      ;; this is a plain old value, not a future. decread the
                      ;; num-bindings to take account
                      (decf ,num-bindings))
                  (when (<= ,num-bindings (hash-table-count track-future-fired))
                    (let ((vars (loop for bind in ',bind-vars collect (getf ,finished-vals bind))))
                      (apply #'finish (append (list ,finished-future) vars))))))))
       ;; for each binding, attach a callback to the future it generates that
       ;; marks itself as complete. once all binding forms report in, the main
       ;; future "finished-future" is triggered, which runs the body
       ,@(loop for (bind form) in bindings collect
           `(let ((future-gen (multiple-value-list ,form)))
              ;; when this future finishes, call the finished-cb, which tallies
              ;; up the number of finishes until it equals the number of
              ;; bindings.
              (attach (apply #'values future-gen)
                (lambda (&rest ,args)
                  (setf (getf ,finished-vals ',bind) (car ,args))
                  (funcall ,finished-cb (car future-gen))))))
       ;; return our future which gets fired when all bindings have completed.
       ;; gets events forwarded to it from the binding futures.
       (attach ,finished-future
         (lambda ,bind-vars
           ,@(progn
               (when ignore-bindings
                 (push `(declare (ignore ,@ignore-bindings)) body))
               body))))))

(defmacro alet* (bindings &body body)
  "Asynchronous let*. Allows calculating a number of values in sequence via
   futures, and run the body when all values have computed with the bindings
   given available to the body.
   
   Also returns a future that fires with the values returned from the body form,
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
            (future (cadr binding))
            (args (gensym "args")))
        (setf body-form
              `((attach ,future
                  (lambda (&rest ,args)
                    (let ((,bind (car ,args)))
                      ,@(progn
                          (when (member bind ignore-bindings)
                            (push `(declare (ignore ,bind)) body-form))
                          body-form))))))))
    (car body-form)))

(defmacro aif (future-gen true-form false-form)
  "Acts like `if` except that the evaluated form accepts a future:
     (aif (async-action)
          (it-worked!)
          (nope-sad-face))"
  (let ((bind (gensym "aif-res")))
    `(alet ((,bind ,future-gen))
       (if ,bind
           ,true-form
           ,false-form))))

(defmacro multiple-future-bind ((&rest bindings) future-gen &body body)
  "Like multiple-value-bind, but instead of a form that evaluates to multiple
   values, takes a form that generates a future."
  (let* ((args (gensym "args"))
         (ignore-bindings nil)
         ;; replace nil bindings with symbols that will be explicitely ignored
         (bindings (loop for binding in bindings
                         collect (if (null binding)
                                     (let ((ignored (gensym "ignored-binding")))
                                       (push ignored ignore-bindings)
                                       ignored)
                                     binding))))
    `(attach ,future-gen
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

(defmacro wait-for (future-gen &body body)
  "Wait for a future to finish, ignoring any values it returns. Can be useful
   when you want to run an async action but don't care about the return value
   (or it doesn't return a value) and you want to continue processing when it
   returns."
  (let ((ignore-var (gensym "async-ignore")))
    `(attach ,future-gen
       (lambda (&rest ,ignore-var)
         (declare (ignore ,ignore-var))
         ,@body))))

(defmacro adolist ((item items &optional future-bind) &body body)
  "Async version of dolist, only continues loop when future in final form
   finishes with a value."
  (let ((items-sym (gensym "items"))
        (future-sym (gensym "future"))
        (next-fn (gensym "next-fn")))
    `(let ((,items-sym ,items)
           (,future-sym (make-future)))
       (labels ((,next-fn ()
                  (let ((,item (car ,items-sym)))
                    (unless ,item
                      (finish ,future-sym nil)
                      (return-from ,next-fn))
                    (setf ,items-sym (cdr ,items-sym))
                    (future-handler-case
                      ,(if future-bind
                           `(let ((,future-bind (make-future)))
                              (wait-for ,future-bind (,next-fn))
                              ,@body)
                           `(wait-for (progn ,@body) (,next-fn)))
                      (t (e) (signal-error ,future-sym e))))))
         (,next-fn))
       ,future-sym)))

(defmacro %handler-case (body &rest bindings)
  "Simple wrapper around handler-case that allows switching out the form to make
   macroexpansion a lot easier to deal with."
  `(handler-case ,body ,@bindings))

(defmacro wrap-event-handler (future-gen error-forms)
  "Used to wrap the future-generation forms of future syntax macros. This macro
   is not to be used directly, but instead by future-handler-case.
   
   It allows itself to be recursive, but any recursions will simply add their
   error forms for a top-level list and return the form they are given as the
   body. This allows a top-level form to add an error handler to a future, while
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
    ;; future-gen form verbatim, but add their error handlers to the error
    ;; handling chain
    `(macrolet ((wrap-event-handler (future-gen error-forms)
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
                       ;; return the future-gen form verbatim
                       ,future-gen))))
       ;; define a function that signals the error, and a top-level error handler
       ;; which uses the error-forms passed to THIS macro instance. any instance
       ;; of `wrap-event-handler` that occurs in the `future-gen` form will inject
       ;; its error handler between handler-fn and signal-error.
       (let* ((,signal-error (lambda (ev) (error ev)))
              (,handler-fn (lambda (ev)
                             (%handler-case
                               (funcall ,signal-error ev)
                               ,@error-forms)))
              ;; sub (wrap-event-handler ...) forms are expanded with ,future-gen
              ;; they add their handler-case forms into a lambda which is injected
              ;; into the error handling chain,
              (,vals (multiple-value-list ,future-gen)))
         (if (futurep (car ,vals))
             (progn
               (attach-errback (car ,vals) ,handler-fn))
             (apply #'values ,vals))))))

(defmacro future-handler-case (body-form &rest error-forms &environment env)
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
   future-handler-case to add layers of error handling around any `attach` call
   that is within lexical grasp."
  (if (find :future-debug *features*)
      ;; we're debugging futures...disable all error handling (so errors bubble
      ;; up to main loop)
      body-form
      ;; wrap the top-level form in a handler-case to catch any errors we may
      ;; have before the futures are even generated.
      `(%handler-case
         ;; redefine our attach macro so that the future-gen forms are
         ;; wrapped (recursively, if called more than once) in the
         ;; `wrap-event-handler` macro.
         (macrolet ((attach (future-gen fn &environment ml-env)
                      (let ((args (gensym "fhc-wrap-args")))
                        ;; call the original attach macro (via our pass env).
                        ;; this allows calling it without throwing macrolet
                        ;; into an endless loop
                        (funcall (macro-function 'attach ',env)
                          `(attach
                             (wrap-event-handler ,future-gen ,',error-forms)
                             ;; create a wrapper function around the given
                             ;; callback that applies our error handlers
                             (lambda (&rest ,args)
                               (%handler-case
                                 (apply ,fn ,args)
                                 ,@',error-forms)))
                          ml-env))))
             ,body-form)
         ,@error-forms)))

