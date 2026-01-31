(asdf:defsystem #:ssz-lisp
  :description "SSZ (Simple Serialize) for Ethereum in Coalton"
  :author "rsl"
  :license "MIT"
  :version "0.1.0"
  :defsystem-depends-on (#:coalton-asdf)
  :depends-on (#:coalton #:ironclad #:cl-yaml #:snappy)
  :serial t
  :components ((:file "src/ssz-helpers")
               (:coalton-file "src/ssz")
               (:file "src/ssz-wrappers")))

(asdf:defsystem #:ssz-lisp/tests
  :description "Tests and CLI for SSZ (Simple Serialize) in Coalton"
  :author "rsl"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:ssz-lisp #:fiveam)
  :serial t
  :components ((:file "test/package")
               (:file "test/ssz-basic")
               (:file "test/ssz-generic")
               (:file "src/ssz-cli")))
