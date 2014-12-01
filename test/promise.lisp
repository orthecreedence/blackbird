(in-package :blackbird-test)
(in-suite blackbird-test)

(defun promise-gen (value-cb &key delay)
  (with-promise (resolve reject :resolve-fn resolve-fn)
    (if delay
        (as:delay (lambda () (apply resolve-fn (multiple-value-list (funcall value-cb))))
                  :event-cb (lambda (e) (reject e))
                  :time delay)
        (apply resolve-fn (multiple-value-list (funcall value-cb))))))

(test make-promise
  "Test that make-promise returns a promise, also test promisep"
  (is (promisep (make-promise)))
  (is (not (promisep 'hai)))
  (is (not (promisep "omg, WHERE did you get those shoes?!")))
  (is (not (promisep nil))))

(test promise-callbacks
  "Test that finishing a promise fires its callbacks, also test multiple callbacks"
  (let ((promise (promise-gen (lambda () 5)))
        (res1 nil)
        (res2 nil))
    (attach promise
      (lambda (x)
        (setf res1 (+ 3 x))))
    (attach promise
      (lambda (x)
        (setf res2 (+ 7 x))))
    (is (= res1 8))
    (is (= res2 12))))

(test promise-errbacks
  "Test that errbacks are fired (also test multiple errbacks)"
  (let ((promise (promise-gen (lambda () (error "omg lol wtf"))))
        (fired1 nil)
        (fired2 nil))
    (attach-errback promise
      (lambda (ev)
        (setf fired1 ev)))
    (attach-errback promise
      (lambda (ev)
        (setf fired2 ev)))
    (signal-error promise 'omg-lol-wtf)
    (is (typep fired1 'error))
    (is (typep fired2 'error))))

(test promisify
  "Make sure promisifying works properly"
  (let ((val1 nil)
        (err1 nil)
        (val2 nil)
        (err2 nil)
        (val3 nil)
        (err3 nil))
    ;; value
    (let ((promise1 (promisify 5)))
      (attach-errback promise1 (lambda (e) (setf err1 e)))
      (attach promise1 (lambda (x) (setf val1 x)))
      (is (promisep promise1)))
    ;; error
    (let ((promise2 (promisify (error "an error"))))
      (attach-errback promise2 (lambda (e) (setf err2 e)))
      (attach promise2 (lambda (x) (setf val2 x)))
      (is (promisep promise2)))
    ;; multiple values
    (let ((promise3 (promisify (values 3 6 7))))
      (attach-errback promise3 (lambda (e) (setf err3 e)))
      (attach promise3 (lambda (&rest vals) (setf val3 vals)))
      (is (promisep promise3)))
    (is (eq val1 5))
    (is (eq err1 nil))
    (is (eq val2 nil))
    (is (typep err2 'error))
    (is (equalp val3 (list 3 6 7)))
    (is (eq err3 nil))))

(test with-promise
  "Test standard promise creation via with-promise"
  ;; value(s)
  (let ((val nil))
    (attach
      (with-promise (resolve reject)
        (resolve 12 5))
      (lambda (&rest vals) (setf val vals)))
    (is (equalp val (list 12 5))))
  ;; reject errors
  (let ((val nil)
        (err nil))
    (let ((promise (with-promise (resolve reject)
                     (reject (make-instance 'error))
                     (resolve 3))))
      (attach promise (lambda (v) (setf val v)))
      (attach-errback promise (lambda (e) (setf err e))))
    (is (eq val nil))
    (is (typep err 'error)))
  ;; caught errors
  (let ((err nil))
    (let ((promise (with-promise (resolve reject)
                     (error "oh crap")
                     (resolve 4))))
      (attach-errback promise (lambda (e) (setf err e))))
    (is (typep err 'error))))

(test promise-alet
  "Test that the alet macro functions correctly"
  (let ((time-start nil))  ; tests that alet bindings happen in parallel
    (multiple-value-bind (val-x val-y)
        (async-let ((val-x nil)
                    (val-y nil))
          (setf time-start (get-internal-real-time))
          (alet ((x (promise-gen (lambda () 5) :delay .2))
                 (y (promise-gen (lambda () 2) :delay .2)))
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
          (alet* ((x (promise-gen (lambda () 5) :delay .2))
                  (y (promise-gen (lambda () (+ 2 x)) :delay .2)))
            (setf val-x x
                  val-y y)))
      (let ((alet*-run-time (/ (- (get-internal-real-time) time-start) internal-time-units-per-second)))
        (is (<= .38 alet*-run-time .42))
        (is (= val-x 5))
        (is (= val-y 7))))))

(test multiple-promise-bind
  "Test multiple-promise-bind macro"
  (multiple-value-bind (name age)
      (async-let ((name-res nil)
                  (age-res nil))
        (multiple-promise-bind (name age)
            (promise-gen (lambda () (values "andrew" 69)) :delay .2)
          (setf name-res name
                age-res age)))
    (is (string= name "andrew"))
    (is (eq age 69))))

(test promise-wait
  "Test wait macro"
  (multiple-value-bind (res1 res2)
      (async-let ((res1 nil)
                  (res2 nil))
        (wait (promise-gen (lambda () nil))
          (setf res1 2))
        (wait (promise-gen (lambda () nil))
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
            (alet ((x (promise-gen (lambda () 'sym1))))
              (+ x 7))
            (type-error (e)
              (setf err1 e)))
          (t (e)
            (declare (ignore e))
            (setf err1 :failwhale)))
        (promise-handler-case
          (promise-handler-case
            (multiple-promise-bind (name age)
                (promise-gen (lambda () (values "leonard" 69)))
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
;; test error propagation/finalizers
;; -----------------------------------------------------------------------------

(test catcher-catch
  "Test catchers will catch errors in the chain."
  (let ((err nil)
        (val nil))
    (catcher
      (attach
        12
        (lambda (x)
          (attach
            (+ x 'uhoh)
            (lambda (x) (setf val x)))))
      (error (e) (setf err e)))
    (is (typep err 'error))
    (is (eq val nil))))
              
(test catcher-passthru
  "Test catchers will forward values if no error is caught."
  (let ((err nil)
        (val nil))
    (attach
      (catcher
        (attach 4 (lambda (x) (+ x 5)))
        (t (e) (setf err e)))
      (lambda (x)
        (setf val x)))
    (is (eq val 9))
    (is (eq err nil))))

(test finally
  "Finally works"
  (let ((val1 nil)
        (val2 nil))
    (finally (with-promise (res rej) (res 12))
      (setf val1 3))
    (finally (with-promise (res rej) (error "dasdf") (res 1))
      (setf val2 7))
    (is (eq val1 3))
    (is (eq val2 7))))

;; -----------------------------------------------------------------------------
;; special var handling
;; -----------------------------------------------------------------------------
(test chaining
  "Test chaining"
  (let ((val nil))
    (chain 4
      (:then (x) (+ x 7))
      (:then (x) (list 3 x 9))
      (:map (x) (+ x 1))
      (:reduce (acc x 0) (+ acc x))
      (:then (final) (setf val final)))
    (is (eq val 26)))
  (let ((err nil)
        (val nil)
        (final nil))
    (chain 4
      (:then (x) (list (+ x 5) 'ttt))
      (:map (x) (+ x 12))
      (:then (x) (+ x 4))
      (:then (x) (setf val x))
      (:catch (e) (setf err e))
      (:finally () (setf final 'lol)))
    (is (typep err 'type-error))
    (is (eq val nil))
    (is (eq final 'lol))))

;; -----------------------------------------------------------------------------
;; promise utils (map, reduce, etc)
;; -----------------------------------------------------------------------------
(test amap
  "Test mapping over a list of vals/promises"
  (let ((vals (with-promise (res rej)
                (res (list 3
                           (with-promise (res rej) (res 4))
                           5))))
        (res nil))
    (attach 
      (amap (lambda (x) (with-promise (res rej) (res (+ x 5)))) vals)
      (lambda (x) (setf res x)))
    (is (equalp res (list 8 9 10)))))

(test all
  "Test waiting on a list of promises/vals"
  (let ((vals (with-promise (res rej)
                (res (list 1
                           (with-promise (res rej) (res 2))
                           (with-promise (res rej) (res 3))))))
        (res nil))
    (attach
      (all vals)
      (lambda (x) (setf res x)))
    (is (equalp res (list 1 2 3)))))

(test areduce
  "Test reducing a list of vals/promises"
  (let ((vals (with-promise (res rej)
                (res (list 1
                           (with-promise (res rej) (res 2))
                           (with-promise (res rej) (res 3))))))
        (res nil))
    (attach
      (areduce (lambda (agg x) (+ agg x))
               vals
               0)
      (lambda (x) (setf res x)))
    (is (eq res 6))))

(test afilter
  "Test filtering on a list of vals/promises"
  (let ((vals (with-promise (res rej)
                (res (list 3
                           (with-promise (res rej) (res 4))
                           5))))
        (res nil))
    (attach 
      (afilter (lambda (x) (with-promise (res rej) (res (oddp x)))) vals)
      (lambda (x) (setf res x)))
    (is (equalp res (list 3 5)))))

;; -----------------------------------------------------------------------------
;; special var handling
;; -----------------------------------------------------------------------------

(defvar *special-var-0*)
(defvar *special-var-1*)
(defvar *special-var-2*)
(defvar *special-var-3*)

(test test-keep-specials
  "Test *promise-keep-specials*"
  (let ((*promise-keep-specials* '(*special-var-0* *special-var-1*
                                   *special-var-2* *special-var-3*))
        (*special-var-1* :one)
        (*special-var-2* :two)
        (promise (make-promise))
        result)
    (labels ((get-vars ()
               (loop for var in *promise-keep-specials*
                     collect (if (boundp var)
                                 (symbol-value var)
                                 :unbound)))
             (verify (finish-func)
               (let ((*special-var-1* nil)
                     (*special-var-2* :foo)
                     (*special-var-3* :bar))
                 (makunbound '*special-var-1*)
                 (funcall finish-func)
                 (is (equal '(:unbound :unbound :foo :bar) (get-vars))))
               ;; unbound state of variables isn't preserved
               ;; across the callback chain
               (is (equal '(:unbound :one :two :unbound) result))))
      (attach promise
              #'(lambda (v)
                  (declare (ignore v))
                  (setf result (get-vars))))
      (verify #'(lambda () (finish promise nil)))
      (attach-errback promise
                      #'(lambda (c)
                          (declare (ignore c))
                          (setf result (get-vars))))
      (verify #'(lambda () (signal-error promise 'omg-lol-wtf))))))

;; -----------------------------------------------------------------------------
;; forced async finishing
;; -----------------------------------------------------------------------------
(test forced-async
  "Test that forced-async finishing works"
  ;; should resolve a value immediately
  (multiple-value-bind (val)
      (async-let ((val nil))
        (let ((promise (with-promise (res rej) (res 12))))
          (attach promise
            (lambda (x) (setf val x)))
          (is (eq val 12))))
    (is (eq val 12)))
  ;; should be nil until it resolves a second later
  (let ((*promise-finish-hook*
          (lambda (finish-fn)
            (as:delay finish-fn :time .1))))
    (multiple-value-bind (val)
        (async-let ((val nil))
          (let ((promise (with-promise (res rej) (res 12))))
            (attach promise
              (lambda (x) (setf val x)))
            (is (eq val nil))))
      (is (eq val 12)))))

;; -----------------------------------------------------------------------------
;; old promise-handler-case test (obsolete)
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

