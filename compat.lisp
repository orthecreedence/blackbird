;;; Define a set of backwards-compatible functions and methods for the future-*
;;; -> promise-* renaming that was previously in cl-async-future.

(in-package :blackbird)

(eval-when (:load-toplevel :compile-toplevel)
  (defun str-replace (string old new)
    "Replace a portion of a string with another."
    (let ((pos (search old string :test 'string=)))
      (if pos
          (str-replace (concatenate 'string (subseq string 0 pos) new (subseq string (+ pos (length old)))) old new)
          string))))

(defmacro with-forwarded (name (promisified) &body body)
  (let ((_str-name (gensym "str-name")))
    `(let* ((,_str-name (string-downcase (string ,name)))
            (,promisified (intern (string-upcase (str-replace ,_str-name "future" "promise")))))
       `(progn
          ,,@body
          ;; would rather these be explicit exports in package.lisp
          ;(export ',,name)
          ))))

(defmacro forward-function (name)
  (with-forwarded name (promisified)
    `(setf (symbol-function ',name) (symbol-function ',promisified))))

(defmacro forward-macro (name)
  (with-forwarded name (promisified)
    `(setf (macro-function ',name) (macro-function ',promisified))))

;; -----------------------------------------------------------------------------
;; let the forwarding begin!
;; -----------------------------------------------------------------------------

(defclass future (promise) ())
(forward-function future-finished-p)
(forward-function make-future)
(forward-function lookup-forwarded-future)
(forward-function futurep)
(forward-function reset-future)
(forward-macro multiple-future-bind)
(forward-macro future-handler-case)

(setf (macro-function 'wait-for) (macro-function 'wait))

