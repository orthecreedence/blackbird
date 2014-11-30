---
title: Documentation
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
  - [make-promise](#make-promise) _function_
  - [attach-errback](#attach-errback) _function_
  - [signal-error](#signal-error) _function_
  - [promisep](#promisep) _function_
  - [finish](#finish) _function_
  - [promise-finished-p](#promise-finished-p) _method_
  - [lookup-forwarded-promise](#lookup-forwarded-promise) _function_
  - [attach](#attach) _macro_
- [Nicer syntax](#nicer-syntax)
  - [alet](#alet) _macro_
  - [alet*](#alet-star) _macro_
  - [aif](#aif) _macro_
  - [multiple-promise-bind](#multiple-promise-bind) _macro_
  - [wait](#wait) _macro_
  - [adolist](#adolist) _macro_
- [Error handling](#error-handling)
  - [promise-handler-case](#promise-handler-case) _macro_
  - [:promise-debug](#promise-debug) _feature_
- [Backwards compatibility](#compat)


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

Our implementation of promises are great for this because of the following
reasons:

- If a callback is [attached](#attach) to a value that is not a [promise](#promise),
that callback is called immediated with the value. This makes it so that you can
attach a callback to anything: a promise or a value, and the end result is the
same. This way, the distiction between CPS and normal, stack-based programming
fades slightly because a function can return a promise or a value, and you can
bind a callback to either.
- Calling [attach](#attach) always returns a promise. This returned promise gets
fired with the *return value of the callback being attached*. So if you have
Promise A and you attach a callback to it, `attach` returns Promise B. Promise B
gets [finished](#finish) with the return value(s) from the callback attached to
Promise A.
- [Finishing](#finish) a promise with another promise as the first value results
in the callbacks/errbacks from the promise being finished transferring over
to the promise that is passed as the value. This, in addition to [attach](#attach)
always returning a promise, makes nesting promises possible. In other words, a
promise can result in a promise which results in a promise, and if the final promise
is finished with a value, the callbacks attached to the first (top-level) promise
will be called with this value. This provides what's almost a call stack for
asynchronous operations in that you can derive a value from deep within a bunch
of CPS calls back to the top-level, assuming that your operations are in the
tail position (remember, a callback has to return the promise from the next
operation for this to work).

This is all probably greek, so let's give an example:

{% highlight cl %}
(use-package :cl-async-promise)

(defun promise-calc (x)
  "Asynchronously add 1 to x, returning a promise that will be finished when x is computed."
  (let ((promise (make-promise)))
    (as:delay (lambda () (finish promise (+ x 1)))
              :time 1)
    promise))

(as:start-event-loop
  (lambda ()
    (let ((promise (attach (promise-calc 0)
                    (lambda (x)           ;; x is 1 here
                      (attach (promise-calc x)
                        (lambda (x)       ;; x is 2 here
                          (attach (promise-calc x)
                            (lambda (x)   ;; x is 3 here
                              (* x 5)))))))))
      (attach promise
        (lambda (x)
          (format t "Final result: ~a" x))))))
{% endhighlight %}

This waits 3 seconds then prints:

    Final result: 15

Notice how the callback was attached to the top-level promise, but was able to
get the result computed from many async-levels deep. Not only does this mimick a
normal call stack a lot closer than CPS, but can be wrapped in macros that make
the syntax almost natural (note that these macros I speak of are on the way).

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

<a id="make-promise"></a>
### make-promise
{% highlight cl %}
(defun make-promise (&key preserve-callbacks (reattach-callbacks t)))
  => promise
{% endhighlight %}

Create a promise. Supports persistent callbacks (can be fired more than once) and
reattaching callbacks to another promise (when this promise is [finished](#finish)
with another promise as the value).

{% highlight cl %}
;; example
(let ((promise (make-promise)))
  (attach promise
    (lambda (x)
      (format t "x is ~a~%" x)))
  (finish promise 5))
{% endhighlight %}

<a id="attach-errback"></a>
### attach-errback
{% highlight cl %}
(defun attach-errback (promise errback))
  => promise
{% endhighlight %}

This adds an "errback" (an error callback) to the promise, to be called whenever
[signal-error](#signal-error) is called on the promise. A promise can hold
multiple errbacks, allowing different pieces of your application to set up
handling for different events a promise might encounter.

When there are no errbacks attached to a promise, any errors triggered on that
promise are saved until an errback is added, at which point the errback is called
with all the saved up errors in the order they were received.

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

<a id="signal-error"></a>
### signal-error
{% highlight cl %}
(defun signal-error (promise condition))
  => nil
{% endhighlight %}

Signal an error on the promise. Many async operations will signal events/errors,
and this allows you to "transfer" these events to a promise. You handle errors
on a promise by [settin up errbacks](#attach-errback) on the promise.

{% highlight cl %}
;; example
(let ((promise (make-promise)))
  ;; send out a request and finish our promise when we get a response, but also
  forward any events get to the promise to the handler can process them
  (tcp-connect "musio.com" 80
    (lambda (sock data)
      (finish promise data))
    (lambda (ev)
      ;; signal the event on the promise
      (signal-error promise ev))
	:data (format nil "GET /~c~c" #\return #\newline))

  ;; attach a callback to the tcp op
  (attach promise
    (lambda (data)
      (format t "got data: ~a~%" (babel:octets-to-string data))))

  ;; handle any events
  (attach-errback promise
    (lambda (ev)
      (format t "ev: ~a~%" ev))))
{% endhighlight %}

<a id="promisep"></a>
### promisep
{% highlight cl %}
(defun promisep (object))
  => t/nil
{% endhighlight %}

Test if the given object is a promise.

<a id="finish"></a>
### finish
{% highlight cl %}
(defun finish (promise &rest values))
  => promise
{% endhighlight %}

Finish a promise with one or more values. When finished, all callbacks attached
to the promise will be fired, with the given values as their arguments. The same
promise passed in is returned.

{% highlight cl %}
;; example
(let ((promise (make-promise)))
  (as:delay (lambda () (finish promise 1 2 3)))
  (attach promise
    (lambda (x y z)
      (format t "result: ~a~%" (* x y z)))))
{% endhighlight %}

<a id="promise-finished-p"></a>
### promise-finished-p
{% highlight cl %}
(defmethod promise-finished-p (promise))
  => T/NIL
{% endhighlight %}

This method returns T or NIL depending on whether the given promise has been
finished.

<a id="lookup-forwarded-promise"></a>
### lookup-forwarded-promise
{% highlight cl %}
(defun lookup-forwarded-promise (promise))
  => promise
{% endhighlight %}

This function takes a promise, follows the chain of forwarding, and returns the
last promise that the given one forwards to. If no forwarding is set up, then it
returns the promise that was passed in.

The purpose of this function is to allow an app to look at, given a promise, what
is the *actual* promise that will be operated on.

<a id="attach"></a>
### attach
{% highlight cl %}
(defmacro attach (promise-gen callback))
  => new-promise
{% endhighlight %}

This macro attaches a callback to a promise such that once the promise computes,
the callback will be called with the promise's finished value(s) as its
arguments.

`attach` takes two arguments, `promise-gen` and `cb`. `promise-gen` is a form that
*can* (but is not required to) return a promise. If the first value of
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
of the given callback once it is fired. So the original promise fires, the
callback gets called, and then the promise that was returned from `attach` is
fired with the return values from the callback.

Also note that if a `promise` is [finished](#finish) with another promise as the
first value, the original promise's callbacks/errorbacks are _transfered_ to
the new promise. This, on top of `attach` always returning a promise, makes
possible some incredible syntactic abstractions which can somewhat mimick non
CPS style by allowing the results from async operations several levels deep to
be viewable by the top-level caller.

{% highlight cl %}
;; example
(attach (my-async-op-which-returns-a-promise)
  (lambda (x)
    (format t "x is ~a~%" x)))
{% endhighlight %}

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

<a id="error-handling"></a>
Error handling
--------------
All the wonderful [syntax macros](#nicer-syntax) in the world aren't going to do
you any good if you can't handle errors and conditions properly. The error
handling for promises closely follows how you would handle errors in native lisp.

<a id="promise-handler-case"></a>
### promise-handler-case
{% highlight cl %}
(defmacro promise-handler-case (body-form &rest error-forms))
  => body-form-return
{% endhighlight %}

This macro wraps any of the above macros (`attach`, `alet`, `alet*`,
`multiple-promise-bind`, `wait-for`) with `handler-case` like error handling.

It works not only on the top-level forms, but also on each form within the above
macros that generates a promise, meaning that a single handler form can set up
error handling for all the sub-forms, even if the stack has unwound.

Note that `promise-handler-case` will only work on the forms it wraps (ie the
lexical scope). If you leave via a function call or similar, it *will only catch
errors that occur in that function if they are generated synchronously*. This is
probably the main difference between `handler-case` and `promise-handler-case`.
If you want to call a function that operates asynchronously from *within* a
`promise-handler-case` macro, make sure that function sets up its own error
handlers.

{% highlight cl %}
;; simple example
(promise-handler-case
  (alet ((record (get-record-from-server)))
    (format t "got record: ~a~%" record))
  (event-error (e)
    (format t "oh no, an error.~%")))

;; nesting example

(defun process-results (x y)
  ;; any errors triggered on this stack will be caught. any errors occuring
  ;; after (calculate-z-from-server ...) returns will NOT NOT NOT be caught.
  (alet ((z (calculate-z-from-server x y)))
    (format t "z is ~a~%" z)))

(promise-handler-case
  (alet ((sock (connect-to-server)))
    (promise-handler-case
      (multiple-promise-bind (id name)
          (get-user-from-server :socket sock)
        (alet* ((x (get-x-from-server :socket sock))
                (y (get-y-from-server :socket sock)))
          (format t "x+y: ~a~%" (+ x y))
          (process-results x y)))
      (type-error (e)
        (format t "Got a type error, x or y possibly isn't a number: ~a~%" e))))
  (tcp-error (e)
    (format t "Error connecting to server: ~a~%" e))
  (event-error (e)
    (format t "Got an error event: ~a~%" e))
  (t (e)
    (format t "Got general error: ~a~%" e)))
{% endhighlight %}

In the above, if `x` or `y` are not returned as numbers, it will be caught by
the `(type-error ...)` handler. If some unknown error occurs anywhere inside the
top-level `promise-handler-case`, the outer `(t (e) ...)` general error handler
will get triggered (even though there's a `promise-handler-case` inside it).

If `process-results` signals an error, it will only be caught by the
`promise-handler-case` forms if it spawned no asynchronous events _or_ if any
errors signaled are done so on the current stack (ie synchronously, *not*
asynchronously).

Really, if you want to call out to another function that performs asynchronous
operations from within a `promise-handler-case`, make sure that function is
perfectly capable of handling its own errors without relying on the calling form
to catch them *OR ELSE*.

<a id="promise-debug"></a>
### :promise-debug
If this keyword is present in `*features*` when compiling your app, *all* promise
error handling is turned off (ie [promise-handler-case](#promise-handler-case)
doesn't catch any errors), and [signal-error](#signal-error) will throw the given
condition (instead of pushing it onto the promise errors array).

This makes it (in some cases) a lot easier to debug an app that has layers upon
layers of async. Sometimes it can be tricky to see where a particular error is
coming from and it makes sense to turn off *all* error handling and just let
things bubble up to the top level.

Note that if you push `:promise-debug` onto `*features*`, you have to recompile
your app (since it works on the macro level).

<a id="compat"></a>
Backwards compatibility
-----------------------
Blackbird is the successor to cl-async-future. In fact, the API is incredibly
similar, except for renaming "future" to "promise" and renaming the `wait-for`
macro to `wait`.

Because a number of libraries depend on cl-async-future but may want to switch
to blackbird (or not), cl-async-future has been re-written to be a compatibility
layer over blackbird. This means that whenever you use cl-async-future, you're
actually using blackbird under the hood. This is imnportant because your app
can use the same promise objects from another app even if you're using
cl-async-future and the other app is using blackbird (the libraries have
interchangable promise objects).

So for users of cl-async-future, there's no upgrade needed. You can continue
using it, and even gain access to incremental improvements that go into
blackbird.

