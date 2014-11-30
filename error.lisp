;;; NOTE: all these utilities are now obsolete. Please use catcher instead of
;;; promise-handler-case!!

(in-package :blackbird)

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
               (attach-errback (car ,vals) ,handler-fn)
               (car ,vals))
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

