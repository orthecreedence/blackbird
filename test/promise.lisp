(in-package :blackbird-test)
(in-suite blackbird-test)

;; TODO: finishing, forwarding, error handling, syntax macros, attach with value
;; vs promise (immediate finish)

(test make-promise
  "Test that make-promise returns a promise, also test promisep"
  (is (promisep (make-promise)))
  (is (promisep (make-promise :preserve-callbacks t :reattach-callbacks nil)))
  (is (not (promisep 'hai)))
  (is (not (promisep "omg, WHERE did you get those shoes?!")))
  (is (not (promisep nil))))

(test promise-callbacks
  "Test that finishing a promise fires its callbacks, also test multiple callbacks"
  (let ((promise (make-promise))
        (res1 nil)
        (res2 nil))
    (attach promise
      (lambda (x)
        (setf res1 (+ 3 x))))
    (attach promise
      (lambda (x)
        (setf res2 (+ 7 x))))
    (finish promise 5)
    (is (= res1 8))
    (is (= res2 12))))

(test promise-errbacks
  "Test that errbacks are fired (also test multiple errbacks)"
  (let ((promise (make-promise))
        (fired1 nil)
        (fired2 nil))
    (attach-errback promise
      (lambda (ev)
        (setf fired1 ev)))
    (attach-errback promise
      (lambda (ev)
        (setf fired2 ev)))
    (signal-error promise 'omg-lol-wtf)
    (is (eq fired1 'omg-lol-wtf))
    (is (eq fired2 'omg-lol-wtf))))

(defun promise-gen (&rest vals)
  (let ((promise (make-promise)))
    (as:delay (lambda () (apply #'finish (append (list promise) vals)))
              :time .2
              :event-cb (lambda (ev) (signal-error promise ev)))
    promise))

(test promise-alet
  "Test that the alet macro functions correctly"
  (let ((time-start nil))  ; tests that alet bindings happen in parallel
    (multiple-value-bind (val-x val-y)
        (async-let ((val-x nil)
                    (val-y nil))
          (setf time-start (get-internal-real-time))
          (alet ((x (promise-gen 5))
                 (y (promise-gen 2)))
            (setf val-x x
                  val-y y)))
      (is (<= .19 (/ (- (get-internal-real-time) time-start) internal-time-units-per-second) .22))
      (is (= val-x 5))
      (is (= val-y 2)))))

(test promise-alet*
  "Test that the alet* macro functions correctly"
  (let ((time-start nil))  ; tests that alet bindings happen in sequence
    (multiple-value-bind (val-x val-y)
        (async-let ((val-x nil)
                    (val-y nil))
          (setf time-start (get-internal-real-time))
          (alet* ((x (promise-gen 5))
                  (y (promise-gen (+ 2 x))))
            (setf val-x x
                  val-y y)))
      (let ((alet*-run-time (/ (- (get-internal-real-time) time-start) internal-time-units-per-second)))
        (is (<= .38 alet*-run-time .42))
        (is (= val-x 5))
        (is (= val-y 7))))))

(test promise-multiple-promise-bind
  "Test multiple-promise-bind macro"
  (multiple-value-bind (name age)
      (async-let ((name-res nil)
                  (age-res nil))
        (multiple-promise-bind (name age)
            (promise-gen "andrew" 69)
          (setf name-res name
                age-res age)))
    (is (string= name "andrew"))
    (is (= age 69))))

(test promise-wait
  "Test wait macro"
  (multiple-value-bind (res1 res2)
      (async-let ((res1 nil)
                  (res2 nil))
        (wait (promise-gen nil)
          (setf res1 2))
        (wait (promise-gen nil)
          (setf res2 4)))
    (is (= res1 2))
    (is (= res2 4))))

(define-condition test-error-lol (error) ())

(test promise-handler-case
  "Test promise error handling"
  (multiple-value-bind (err1 err2)
      (async-let ((err1 nil)
                  (err2 nil))
        (promise-handler-case
          (promise-handler-case
            (alet ((x (promise-gen 'sym1)))
              (+ x 7))
            (type-error (e)
              (setf err1 e)))
          (t (e)
            (declare (ignore e))
            (setf err1 :failwhale)))
        (promise-handler-case
          (promise-handler-case
            (multiple-promise-bind (name age)
                (promise-gen "leonard" 69)
              (declare (ignore name age))
              (error (make-instance 'test-error-lol)))
            (type-error (e)
              (setf err2 e)))
          (t (e)
            (setf err2 e))))
    (is (subtypep (type-of err1) 'type-error))
    (is (subtypep (type-of err2) 'test-error-lol))))

(test forwarding
  "Test promise forwarding"
  (flet ((get-val ()
           (alet ((x 4))
             (alet ((y (+ x 4)))
               (+ x y)))))
    (alet ((res (get-val)))
      (is (= res 12)))))


;; -----------------------------------------------------------------------------
;; test error propagation
;; -----------------------------------------------------------------------------

(define-condition promise-error (type-error)
  ((msg :initarg :msg :reader promise-errmsg :initform nil))
  (:report (lambda (c s) (format s "Error event: ~a" (promise-errmsg c)))))

(defun fdelay (val)
  (let ((promise (make-promise)))
    (finish promise (+ val 1))
    promise))

(defmacro defafun (name (promise-bind) args &body body)
  `(defun ,name ,args
     (let ((,promise-bind (make-promise)))
       (promise-handler-case
         (progn ,@body)
         (t (e)
           (signal-error ,promise-bind e)))
       ,promise-bind)))

(defafun async2 (promise) (a)
  (alet* ((z (fdelay a))
          (b (+ z 1)))
    (error 'promise-error :msg "\"5\" is no expected type NUMBER")
    (finish promise (+ b 6))))

(defafun async1 (promise) (a)
  (alet* ((x (fdelay a))
          (y (async2 x)))
    (finish promise y)))

(test error-propagation
  "Test error propagation"
  (let ((error-triggered nil))
    (promise-handler-case
      (wait (async1 1)
        (setf error-triggered nil))
      (t (e)
        (setf error-triggered t)
        (is (typep e 'type-error))))
    (is (eq error-triggered t))))

