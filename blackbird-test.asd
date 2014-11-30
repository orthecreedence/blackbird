(asdf:defsystem blackbird-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2"
  :description "TESTS FOR blackbird."
  :depends-on (#:cl-async #:blackbird #:fiveam)
  :components
  ((:module test
    :serial t
    :components ((:file "util")
                 (:file "promise")
                 (:file "run")))))

