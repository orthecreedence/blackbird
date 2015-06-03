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
  (is (promisep (blackbird-base:make-promise)))
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

(test with-promise-values
  "Test that with-promise can handle being passed multiple values, or one form
   that evaluates to multiple values."
  (let ((vals nil))
    (attach (with-promise (resolve reject)
              (resolve 1 (list 1 2) 3))
      (lambda (&rest promise-vals)
        (setf vals promise-vals)))
    (is (equalp vals '(1 (1 2) 3))))
  (let ((vals nil))
    (attach (with-promise (resolve reject)
              (resolve (funcall (lambda () (values 1 (list 1 2) 3)))))
      (lambda (&rest promise-vals)
        (setf vals promise-vals)))
    (is (equalp vals '(1 (1 2) 3)))))

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

(test promise-walk
  "Test walk macro"
  (multiple-value-bind (vals)
      (async-let ((vals nil))
        (attach
          (walk
            (promise-gen (lambda () (push 1 vals)) :delay .2)
            (promise-gen (lambda () (push 2 vals)) :delay .1)
            (promise-gen (lambda () (values 1 2 3)) :delay .1))
          (lambda (&rest v)
            (push v vals))))
    (is (equalp vals (list (list 1 2 3) 2 1)))))

(test promise-walk1
  "Test walk macro"
  (multiple-value-bind (vals)
      (async-let ((vals nil))
        (attach
          (walk1 (promise-gen (lambda () (values 3 2 1)) :delay .1)
            (promise-gen (lambda () (push 1 vals)) :delay .2)
            (promise-gen (lambda () (push 2 vals)) :delay .1)
            (promise-gen (lambda () (push 3 vals)) :delay .1))
          (lambda (&rest v)
            (push v vals))))
    (is (equalp vals (list (list 3 2 1) 3 2 1)))))


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

(test debug-on-error
  "Test that *debug-on-error* changes the way error handling operates."
  (let ((blackbird:*debug-on-error* t)
        err)
    (handler-case
      (with-promise (res rej)
        (error "oh no"))
      (error (e) (setf err e)))
    (is (subtypep (type-of err) 'error) "when t"))
  (let ((blackbird:*debug-on-error* nil)
        err)
    (catcher
      (with-promise (res rej)
        (error "oh no"))
      (error (e) (setf err e)))
    (is (subtypep (type-of err) 'error) "when nil")))

(test tapping
  "Tap tap taparoo"
  (let ((tapped nil)
        (val nil))
    (attach
      (tap (attach (promisify 3)
             (lambda (x)
               (+ x 4)))
        (lambda (x)
          (setf tapped x)
          52))
      (lambda (x)
        (setf val x)))
    (is (eq val 7))
    (is (eq tapped 7))))

(test tapping-error-passthru
  "Tapping should pass errors through."
  (let ((err nil)
        (tapped nil))
    (catcher
      (tap
        (attach (promisify 4)
          (lambda (x)
            (+ x 'nope)))
        (lambda (omg)
          (setf tapped omg)))
      (t (e) (setf err e)))
    (is (typep err 'type-error))
    (is (null tapped))))

(test tap-finishes-before-continue
  "Make sure tapping waits on its romise before continuing the chain."
  (let ((val nil))
    (as:with-event-loop ()
      (attach
        (tap
          (attach
            (promise-gen (lambda () 4) :delay .1)
            (lambda (x) (+ x 4)))
          (lambda (x)
            (declare (ignore x))
            (promise-gen
              (lambda ()
                (unless val (setf val 'tapped))
                'tap)
              :delay .1)))
        (lambda (x)
          (declare (ignore x))
          (unless val (setf val 'attached)))))
    (is (eq val 'tapped))))

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
  (let ((val nil)
        (final nil))
    (chain 4
      (:then (x) (+ x 7))
      (:then (x) (list 3 x 9))
      (:map (x) (+ x 1))
      (:reduce (acc x 0) (+ acc x))
      (:then (final) (setf val final))
      (:finally (setf final 4)))
    (is (eq val 26))
    (is (eq final 4)))
  (let ((err nil)
        (val nil)
        (final nil))
    (chain 4
      (:then (x) (list (+ x 5) 'ttt))
      (:map (x) (+ x 12))
      (:then (x) (+ x 4))
      (:then (x) (setf val x))
      (:catch (e) (setf err e))
      (:finally (setf final 'lol)))
    (is (typep err 'type-error))
    (is (eq val nil))
    (is (eq final 'lol))))

;; -----------------------------------------------------------------------------
;; promise utils (map, reduce, etc)
;; -----------------------------------------------------------------------------
(test aeach
  "Test looping over items in sequence"
  (multiple-value-bind (items)
    (async-let ((items nil))
      (let ((vals (list (cons 1 .6)
                        (cons 2 .1)
                        (promisify (cons 3 .3)))))
        (aeach (lambda (x)
                 (attach (promise-gen (lambda () (car x)) :delay (cdr x))
                   (lambda (y)
                     (push y items))))

               vals)))
    (is (equalp '(3 2 1) items))))

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

(test amap-nil
  "Test that amap will reslve with a nil list"
  (let ((vals nil)
        (res nil))
    (attach
      (amap (lambda (x) (with-promise (res rej) (res (+ x 5)))) vals)
      (lambda (x) (declare (ignore x)) (setf res t)))
    (is (eq res t))))

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
        (promise (blackbird-base:make-promise))
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
      (verify #'(lambda () (blackbird-base:finish promise nil)))
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

(test async-error
  "Test proper handling of async errors."
  ;; NOTE that we're testing an error that cropped up when reattaching errbacks
  ;; from one promise to another
  (let ((*promise-finish-hook*
          (lambda (finish-fn)
            (as:delay finish-fn :time .1)))
        (val nil)
        (err nil))
    (flet ((do-reject ()
             (with-promise (resolve reject)
               (as:with-delay ()
                 (reject (make-instance 'error)))))
           (do-resolve ()
             (with-promise (resolve reject)
               (as:with-delay ()
                 (resolve 1)))))
      (as:with-event-loop ()
        (catcher
          (attach (attach (do-resolve)
                    (lambda (omg)
                      (declare (ignore omg))
                      (do-reject)))
            (lambda (test-val)
              (setf val test-val)))
          (t (e) (setf err e)))))
    (is (eq val nil))
    (is (typep err 'error))))

(test catch-then
  (multiple-value-bind (log)
      (async-let ((log '()))
        (bb:chain
          (bb:with-promise (resolve reject)
            (reject (make-instance 'error)))
          (:catch (c)
            (push (list :caught1 (type-of c)) log)
            42)
          (:then (x)
            (push (list :ok x) log))
          (:catch (c)
            (push (list :caught2 (type-of c)) log))))
    (is (equal '((:caught1 error) (:ok 42))
               (nreverse log)))))

(test catch-then/delayed
  (multiple-value-bind (log)
      (async-let ((log '()))
        (bb:chain
          (bb:with-promise (resolve reject)
            (as:with-delay ()
              (reject (make-instance 'error))))
          (:catch (c)
            (push (list :caught1 (type-of c)) log)
            42)
          (:then (x)
            (push (list :ok x) log))
          (:catch (c)
            (push (list :caught2 (type-of c)) log))))
    (is (equal '((:caught1 error) (:ok 42))
               (nreverse log)))))

(test attach-error-handle/delayed
  (let ((err nil))
    (as:with-event-loop ()
      (bb:catcher
        (attach (bb:create-promise
                  (lambda (resolver rejecter)
                    (as:delay resolver :event-cb rejecter)))
          (lambda ()
            (error "crap")))
        (t (e) (setf err e))))
    (is (typep err 'error))))

(test long-forward-chain
  (multiple-value-bind (log)
      (async-let ((log '()))
        (let ((n 0))
          (flet ((next-step ()
                   (push (incf n) log)
                   (bb:attach
                    nil
                    #'(lambda (x)
                        (declare (ignore x))
                        (with-promise (resolve reject)
                          (as:with-delay ()
                            (resolve)))))))
            (bb:wait
                (bb:wait (next-step)
                  (next-step))
              (next-step)))))
    (is (equal '(1 2 3) (nreverse log)))))

(test forwarding-stops-finishing
  "If promise A forwards to promise B, promise A can no longer be finished or
   errored."
  (let ((val nil))
    (as:with-event-loop ()
      (bb:chain (bb:with-promise (resolve reject)
                  (as:with-delay (.1)
                    (let ((promise (with-promise (res1 rej1)
                                     (as:with-delay (.1)
                                       (resolve 45)
                                       (res1 12)))))
                      (resolve promise))))
        (:then (val1)
          (unless val (setf val val1)))
        (:catch (err)
          (setf val err))))
    (is (eq val 12))))

