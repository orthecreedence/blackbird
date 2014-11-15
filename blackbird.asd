(asdf:defsystem blackbird
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.5.0"
  :description "A promise implementation for Common Lisp."
  :depends-on ()
  :components
  ((:file "package")
   (:file "promise" :depends-on ("package"))
   (:file "compat" :depends-on ("promise" "package"))))

