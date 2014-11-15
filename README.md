blackbird
=========
A standalone promise implementation for Common Lisp. This is the successor to
the now-deprecated cl-async-future project.

Its goal is to provide an abstraction around synchronous and asynchronous code
in a way that's natural to regular lisp programming. This is ideal for both
writing drivers that can handle both blocking and non-blocking sockets, as well
as various threading operations that have the concept of a delayed result.

[Documentation](http://orthecreedence.github.com/blackbird)
-----------------------------------------------------------------
Check out the [blackbird doc site](http://orthecreedence.github.com/blackbird).

Tests
-----
The test suite can be run by doing the following:

```common-lisp
(ql:quickload :blackbird-test)
(blackbird-test:run-tests)
```

License
-------
MIT.
