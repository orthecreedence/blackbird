(asdf:defsystem cl-async-future-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2"
  :description "TESTS FOR cl-async-future."
  :depends-on (#:cl-async #:cl-async-future #:eos)
  :components
  ((:module test
    :serial t
    :components ((:file "util")
                 (:file "future")
                 (:file "run")))))

