(asdf:defsystem cl-async-future
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.4.2"
  :description "A futures implementation for Common Lisp. Plugs in nicely to cl-async."
  :depends-on ()
  :components
  ((:file "package")
   (:file "future" :depends-on ("package"))))

