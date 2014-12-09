---
title: Blackbird promise library
layout: documentation
---

Blackbird
=========

Blackbird is a promise implementation for Common Lisp. Its purpose is to make
asynchronous operations (such as non-blocking IO or background jobs) simpler to
comprehend and manage.

On top of promises, it provides a number of macros to make using promises as
natural as writing normal lisp code. This lowers the mental barrier to using
asynchronous methods in your app and makes it easier to understand what your
code is doing.

- [Intro to promises](#intro)
- [Promises API](#promise-api)
  - [promise](#promise) _class_
  - [promisep](#promisep) _function_
  - [promise-finished-p](#promise-finished-p) _method_
  - [create-promise](#create-promise) _function_
  - [with-promise](#with-promise) _macro_
  - [promisify](#promisify) _macro_
  - [attach](#attach) _macro_
  - [catcher](#catcher) _macro_
  - [finally](#finally) _macro_
- [Nicer syntax](#nicer-syntax)
  - [alet](#alet) _macro_
  - [alet*](#alet-star) _macro_
  - [aif](#aif) _macro_
  - [multiple-promise-bind](#multiple-promise-bind) _macro_
  - [wait](#wait) _macro_
- [Utils](#utils)
  - [adolist](#adolist) _macro_
  - [amap](#amap) _function_
  - [all](#all) _function_
  - [areduce](#areduce) _function_
  - [afilter](#afilter) _function_
  - [tap](#tap) _function_
  - [chain](#chain) _function_


<a id="intro"></a>
Intro to promises
----------------
A promise is a representation of a value that may exist sometime in the future.
The idea is that you can attach actions to a promise that will run once its
value is computed, and also attach error handlers to make sure any problems are
handled along the way.

Promises not only give an important abstraction for asynchronous programming, but
offer opportunities for [syntactic abstraction](#nicer-syntax) that make async
programming a lot more natural.

The blackbird promise implementation supports the concept of promise chaining,
meaning values and errors that happen in your computations become available to
other promises as they progress, allowing you to program naturally even if
you're doing async operations (which are traditionally callback-based). Here's
how it's done:

- If a callback is [attached](#attach) to a value that is not a [promise](#promise),
that callback is called immediated with the value. This makes it so that you can
attach a callback to anything: a promise or a value, and the end result is the
same. This way, the distiction between CPS and normal, stack-based programming
fades slightly because a function can return a promise or a value, and you can
bind a callback to either.
- Calling [attach](#attach), [catcher](#catcher), or [finally](#finally)
always returns a promise. This returned promise gets
fired with the *return value of the callback being attached*. So if you have
Promise A and you attach a callback to it, `attach` returns Promise B. Promise B
gets finished/resolved with the return value(s) from the callback attached to
Promise A.
- Finishing/resolving a promise with another promise as the first value results
in the callbacks/errbacks from the promise being finished transferring over
to the promise that is passed as the value. This, in addition to
[attach](#attach)/[catcher](#catcher)/[finally](#finally)
always returning a promise, makes chaining promises possible. In other words, a
promise can result in a promise which results in a promise, and if the final promise
is finished with a value, the callbacks attached to the first (top-level) promise
will be called with this value. This provides what's almost a call stack for
asynchronous operations in that you can derive a value from deep within a bunch
of CPS calls back to the top-level, assuming that your operations are in the
tail position (remember, a callback has to return the promise from the next
operation for this to work).

This is all probably greek, so let's give an example:

{% highlight cl %}
(use-package :blackbird)

(defun promise-calc (x)
  "Asynchronously add 1 to x, returning a promise that will be finished when x is computed."
  (with-promise (resolve reject)
    (as:delay (lambda () (resolve (+ x 1))) :time 1)))

(as:with-event-loop ()
  (let ((promise (attach (promise-calc 0)
                   (lambda (x)           ;; x is 1 here
                     (attach (promise-calc x)
                       (lambda (x)       ;; x is 2 here
                         (attach (promise-calc x)
                           (lambda (x)   ;; x is 3 here
                             (* x 5)))))))))
    (attach promise
      (lambda (x)
        (format t "Final result: ~a" x)))))
{% endhighlight %}

This waits 3 seconds then prints:

    Final result: 15

Notice how the callback was attached to the top-level promise, but was able to
get the result computed from many async-levels deep. Not only does this mimick a
normal call stack a lot closer than CPS, but can be wrapped in macros that make
the syntax almost natural (note that these macros I speak of are on the way).

It's important to note that a promise can hold either one set of value(s) or one
error. It cannot hold both, and once it has either a value (or multiple values)
or an error attached to it, the promise essentially becomes read-only.

<a id="promise-api"></a>
Promises API
----------
<a id="promise"></a>
### promise (class)
The promise class represents a promise value. For your application, it's mostly
an opaque object which can be operated on using the functions/macros below. It
currently has no public accessors, and mainly just holds callbacks, errbacks,
values, events, etc.

The standard way to create a promise is with [make-promise](#make-promise).

<a id="promisep"></a>
### promisep
{% highlight cl %}
(defun promisep (object))
  => t/nil
{% endhighlight %}

Test if the given object is a promise.

<a id="promise-finished-p"></a>
### promise-finished-p
{% highlight cl %}
(defmethod promise-finished-p (promise))
  => T/NIL
{% endhighlight %}

This method returns T or NIL depending on whether the given promise has been
finished.

<a id="create-promise"></a>
### create-promise
{% highlight cl %}
(defun create-promise (create-fn &key name))
  => promise
{% endhighlight %}
Creates and returns a new promise using the given `create-fn`, which is a
function of exactly two arguments: a function that can be called with any number
of values and *finishes* the returned promise, or a function of one argument
that signals an error on the promise:

{% highlight cl %}
(create-promise
  (lambda (resolver rejecter)
    ;; note that create-promise will catch errors in the form for you, so this
    ;; handler-case isn't strictly necessary but gives an example of promise
    ;; rejection
    (handler-case
      (let ((my-vals (multiple-value-list (my-operation))))
        (apply resolver my-vals))
      (t (e) (funcall rejecter e)))))
{% endhighlight %}

This syntax can be a little cumbersome, so see [with-promise](#with-promise),
which cleans things up a bit for you.

<a id="with-promise"></a>
### with-promise
{% highlight cl %}
(defmacro with-promise ((resolve reject
                           &key (resolve-fn (gensym "resolve-fn"))
                                (reject-fn (gensym "reject-fn"))
                                name)
                           &body body))
  => promise
{% endhighlight %}

Much like, [create-promise](#create-promise), but wraps promise creation in a
nicer form. This is the recommended way to create promises, unless you for some
reason need the lower-level API of `create-promise`.

Here's an example usage:

{% highlight cl %}
(with-promise (resolve reject)
  (handler-case
    (resolve (my-operation))
    (t (e) (reject e))))
{% endhighlight %}

If you need resolve/reject *functions* to apply in `with-promise`, you can
access them by passing `:resolve-fn` or `:reject-fn`:

{% highlight cl %}
(with-promise (resolve reject :resolve-fn resolver)
  (handler-case
    (let ((vals (multiple-value-list (my-operation))))
      (apply resolver vals))
    (t (e) (reject e))))
{% endhighlight %}

<a id="promisify"></a>
### promisify
{% highlight cl %}
(defmacro promisify (promise-gen))
  => promise
{% endhighlight %}

Given any value(s) (or a triggered error), returns a promise either finished
with those value(s) or failed with the given error:
{% highlight cl %}
(promisify 3)                    ; finished promise with the value 3
(promisify (values "jerry" 28))  ; finished promise with the values "jerry" and 28
(promisify (error "oh no"))      ; failed promise with the error "oh no"
{% endhighlight %}

<a id="attach"></a>
### attach
{% highlight cl %}
(defmacro attach (promise-gen callback))
  => new-promise
{% endhighlight %}

This macro attaches a callback to a promise such that once the promise computes,
the callback will be called with the promise's finished value(s) as its
arguments.

`attach` takes two arguments, `promise-gen` and `callback`. `promise-gen` is a
form that *can* (but is not required to) return a promise. If the first value of
`promise-gen`'s return values is a promise, the callback given is attached to that
promise to be fired when the promise's value(s) are finished.  If the first item
in `promise-gen` is *not* a `promise` class, the _given callback is fired
instantly with the values passed as `promise-values` as the arguments_.

The reason `attach` fires the callback instantly is that it's sometimes nice to
attach a callback to a value when you don't know whether the value is a promise
or an already-computed value. This allows for some useful syntactic
abstractions.

If `attach` is called on a promise that has already been finished, it fires
the given callback immediately with the promise's value(s).

`attach` returns one value: a promise that is finished with the return values
of the given callback once it completes. So the original promise fires, the
callback gets called, and then the promise that was returned from `attach` is
resolved with the return values from the callback.

Also note that if a `promise` is finished/resolved with another promise as the
first value, the original promise's callbacks/errorbacks are _transfered_ to
the new promise. This, on top of `attach` always returning a promise, makes
possible some nice syntactic abstractions which can somewhat mimick non CPS
style by allowing the results from async operations several levels deep to be
viewable by the top-level caller.

{% highlight cl %}
;; example
(attach (my-async-op-which-returns-a-promise)
  (lambda (x)
    (format t "x is ~a~%" x)))
{% endhighlight %}

<a id="catcher"></a>
### catcher
{% highlight cl %}
(defmacro catcher (promise-gen &rest handler-forms))
  => new-promise
{% endhighlight %}

The `catcher` macro is used for catching errors on your promises. It listens for
any errors on your promise chain and allows you to set up handlers for them. The
handler syntax is very much like cl's `handler-case`:

{% highlight cl %}
(catcher
  (attach (my-dangerous-op)
    (lambda (x)
      (format t "danger is my middle name: ~a~%" x)))
  (type-error (e)
    (format t "got a type error! ~a~%" e))
  (t (e)
    (format t "unknown error: ~a~%" e)))
{% endhighlight %}

Notice that like [attach](#attach), `catcher` returns a new promise that
resolves to either

- the value of the promise it wrapped, in the case no error happened
- the value returned by its handler form, if an error occurred

<a id="finally"></a>
### finally
{% highlight cl %}
(defmacro finally (promise-gen &body body))
  => new-promise
{% endhighlight %}

The `finally` macro works much like [attach](#attach) and [catcher](#catcher) in
that it attaches to a promise and resolves the promise it returns to the value
of its body form. However, its body form is called whether or not the given
promise resolves or is rejected...think of it as the `unwind-protect` equivalent
of promises. Its form runs whether the promise chain has an error or a value.
It can be used to close connections or clean up resources that are no longer
needed.

{% highlight cl %}
(let ((conn nil))
  (finally
    ;; catch any db errors
    (catcher
      ;; grab a connection and get some users
      (attach (get-db-connection)
        (lambda (db)
          (setf conn db)
          (attach (get-users-from-db conn)
            (lambda (users)
              (do-something-with-users users)))))
      (connection-failed (e)
        (format t "bad connection! ~a~%" e))
      (wrong-db (e)
        (format t "i'm sorry, but the users are in another db ~a~%" e))
      (t (e)
        (format t "unknown error: ~a~%" e)))
    ;; always close the db, rain or shine
    (when conn
      (close-db-conn))))
{% endhighlight %}

Note that we can use `finally` to close up the database whether things went well
or not. Also note how annoying it is to type out all those `attach` calls. Fear
not, [better syntax awaits](#nicer-syntax).

{% comment %}
-------------------------
<a id="attach-errback"></a>
### attach-errback
{% highlight cl %}
(defun attach-errback (promise errback))
  => promise
{% endhighlight %}

This adds an "errback" (an error handler) to the promise, to be called whenever
an error occurs on the promise. A promise can hold multiple errbacks, allowing
different pieces of your application to set up handling for different events a
promise might encounter.

When there are no errbacks attached to a promise, any error triggered on that
promise is saved until an errback is added, at which point the errback is called
with that error.

{% highlight cl %}
;; example
(let ((promise (make-promise))
      (socket (tcp-connect "musio.com" 80)))
  ;; set up our error/event handler
  (attach-errback promise
    (lambda (ev)
      (handler-case (error ev)
        (tcp-eof () (format t "peer closed socket.~%"))
        (tcp-timeout () (format t "connection timed out.~%"))
        (t () (format t "other event: ~a~%" ev)))))

  ;; write out our request
  (write-socket-data socket (format nil "GET /~c~c" #\return #\newline)
    :read-cb (lambda (sock data) (finish promise data)))

  ;; attach a cb to our heroic promise
  (attach promise
    (lambda (data)
      (format t "got data: ~a~%" (babel:octets-to-string data)))))
{% endhighlight %}
-------------------------
{% endcomment %}


<a id="nicer-syntax"></a>
Nicer syntax
------------
Promises are a great abstraction not only because of the decoupling of an action
and a callback, but also because they can be wrapped in macros to make syntax
fairly natural. The following macros aim to be as close to native lisp as
possible while dealing with asynchronous operations.

<a id="alet"></a>
### alet
{% highlight cl %}
(defmacro alet (bindings &body body))
  => new-promise
{% endhighlight %}

This macro allows `(let)` syntax with async functions that return promises. It
binds the promise return values to the given bindings (in parallel), then runs
the body when all the promises have finished.

It's important to note that `alet` returns a promise from its form, meaning it
can have a callback [attached to it](#attach), just like any other
promise-generating form.

Also know that the binding forms do not not *not* have to return a promise for
the binding process to work. They can return any value, and that variable will
just be bound to that value.

If an `alet` binding form results in multiple values, the first value will be 
bound to the variable (just like `let`).

{% highlight cl %}
;; example (x and y compute in parallel)
(alet ((x (grab-x-from-server1))
       (y (grab-y-from-server2)))
  (format t "x + y = ~a~%" (+ x y)))

;; alet can bind to nil, meaning that the promise is run, but the result is
;; thrown out
(alet ((x (grab-x-from-server))
       (nil (run-command-i-dont-need-the-return-val-for)))
  (format t "got: ~a~%" x))
{% endhighlight %}

__Note:__ `alet` is a useful tool for running operations in parallel, however
use caution when running multiple commands on the same socket, since many
drivers will get confused as to which response goes to which request. Sometimes
opening N connections is easier than trying to match request/response pairs.

<a id="alet-star"></a>
### alet*
{% highlight cl %}
(defmacro alet* (bindings &body body))
  => new-promise
{% endhighlight %}

This macro allows `(let*)` syntax with async functions that return promises. It
binds the promise return values to the given bindings (in sequence), allowing
later bindings to be able to use the values from previous bindings, and then
runs the body when all promises have calculated.

It's important to note that `alet*` returns a promise from its form, meaning it
can have a callback [attached to it](#attach), just like any other
promise-generating form.

Also know that the binding forms do not not *not* have to return a promise for
the binding process to work. They can return any value, and that variable will
just be bound to that value.

If an `alet*` binding form results in multiple values, the first value will be 
bound to the variable (just like `let*`).

{% highlight cl %}
;; example (note we calculate uid THEN name)
(alet* ((uid (grab-user-id-from-server))
        (name (get-user-name-from-id uid)))
  (format t "Dear, ~a. Please return my pocket lint you borrowed from me. My grandfather gave it to me and it is very important. If you do not see fit to return it, be prepared to throw down. Seriously, we're going to throw down and I'm going to straight wreck you.~%" name))

;; alet* can bind to nil, meaning that the promise is run, but the result is
;; thrown out
(alet* ((x (grab-x-from-server))
        (nil (save-val x)))
  (format t "got: ~a~%" x))
{% endhighlight %}

<a id="aif"></a>
### aif
{% highlight cl %}
(defmacro aif (promise-gen true-form false-form))
  => new-promise
{% endhighlight %}

This macro provides `if` for asynchronous values. It is a very simple wrapper
around `alet` that provides a nice syntax for making decisions based on what a
promise will return:

{% highlight cl %}
;; `grab-user-from-db` can return a promise here. if the promise is finished with
;; any value other than NIL, "User exists!" will be printed. If NIL, then "User
;; does not exist..." will print.
(aif (my-app:grab-user-from-db user-id)
     (format t "User exists!~%")
     (format t "User does not exist...~%"))
{% endhighlight %}

<a id="multiple-promise-bind"></a>
### multiple-promise-bind
{% highlight cl %}
(defmacro multiple-promise-bind ((&rest bindings) promise-gen &body body))
  => new-promise
{% endhighlight %}

Like `multiple-value-bind` but for promises. Allows wrapping around a promise that
finishes with multiple values.

It's important to note that `multiple-promise-bind` returns a promise, meaning it
can have a callback [attached to it](#attach), just like any other
promise-generating form.

Also note that the `promise-gen` value does not have to evaluate to a promise, but
any value(s), and the bindings will just attach to the given value(s) (in which
case it works exactly like `multiple-value-bind`.

{% highlight cl %}
;; example
(multiple-promise-bind (id name)
    (get-user-from-server)  ; returns a promise
  (format t "Hai, ~a. Your ID is ~a.~%" name id))
{% endhighlight %}

<a id="wait"></a>
### wait
{% highlight cl %}
(defmacro wait (promise-gen &body body))
  => new-promise
{% endhighlight %}

Wait on a promise without using any of the return values. This is good if you
want to know when an operation has finished but don't care about the result.

{% highlight cl %}
;; example: run-command can return a promise
(wait (run-command)
  (format t "Command finished.~%"))
{% endhighlight %}

<a id="utils"></a>
Utils
-----
The following utility functions allow us to easily perform operations over lists
of values/promises, or promises of lists of values/promises (it's turtles all
the way down).

<a id="adolist"></a>
### adolist
{% highlight cl %}
(defmacro adolist ((item items &optional promise-bind) &body body))
  => new-promise
{% endhighlight %}

This macro allows looping over items in an async fashion. Since it can be tricky
to iterate over a set of results that each does async processing but need to
happen in sequence, this macro abstracts all this away.

Here are some toy examples:

{% highlight cl %}
;; define a timer function
(defun mydelay (time)
  (let ((promise (make-promise)))
    (format t "delay for ~a~%" time)
    (as:delay (lambda () (finish promise)) :time time)
    promise))

;; loop over a list of integers, waiting for each one to finish before
;; triggering the next one.
;;
;; this prints:
;;   delay for 1  (1s pause)
;;   delay for 2  (2s pause)
;;   delay for 3  (3s pause)
;;   DONE!
(wait-for (adolist (item '(1 2 3))
            (mydelay item))
  (format t "DONE!~%"))

;; to get more control over how the promise finishes, specify the promise-bind arg
(adolist (item '(1 2 3) promise)
  (wait-for (mydelay item)
    ;; continue the loop explicitely
    (finish promise)))
{% endhighlight %}

<a id="amap"></a>
### amap
{% highlight cl %}
(defun amap (function promise-list))
  => new-promise
{% endhighlight %}

Maps over a list of values (or promises of values) and runs the given function
on each value, resolving its returned promise as the list of mapped values. Note
they if `function` returns a promise, `amap` waits for the promise to resolve
before continuing, allowing you to run promise-returning operations on all the
values in the collection.

Note that an error on *any* of the promises being iterated will signal an error
on the promise returned from `amap`.

{% highlight cl %}
(attach
  (amap (lambda (x)
          (promisify (+ x 7)))
        (promisify (list 1 2 (promisify 3))))
  (lambda (x)
    (format t "x is: ~a~%" x)))  ; prints "x is (8 9 10)"
{% endhighlight %}

<a id="all"></a>
### all
{% highlight cl %}
(defun all (promise-list))
  => new-promise
{% endhighlight %}

Waits on all of the given values in the `promise-list` to complete before
resolving with the list of all computed values. Like [amap](#amap), the `promise-list`
can be a promise to a list of promises. All will be resolved before continuing.

Note that an error on *any* of the promises being iterated will signal an error
on the promise returned from `amap`.

{% highlight cl %}
(attach (all (promisify (list (promisify 1) (promisify 2) 3)))
  (lambda (vals)
    (format t "vals: ~a~%" vals)))  ; prints "vals: (1 2 3)"
{% endhighlight %}

<a id="areduce"></a>
### areduce
coming soon

<a id="afilter"></a>
### afilter
coming soon

<a id="tap"></a>
### tap
coming soon

<a id="chain"></a>
### chain
coming soon

