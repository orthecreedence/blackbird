(asdf:defsystem blackbird
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.5.2"
  :description "A promise implementation for Common Lisp."
  :depends-on (#:vom)
  :components
  ((:file "package")
   (:file "syntax" :depends-on ("package"))
   (:file "promise" :depends-on ("package"))
   (:file "util" :depends-on ("package" "promise" "syntax"))))

