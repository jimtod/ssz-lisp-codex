(asdf:defsystem #:ssz-lisp
  :description "SSZ (Simple Serialize) for Ethereum in Coalton"
  :author "rsl"
  :license "MIT"
  :version "0.1.0"
  :defsystem-depends-on (#:coalton-asdf)
  :depends-on (#:coalton #:fiveam #:ironclad #:cl-yaml)
  :serial t
  :components ((:file "src/ssz-helpers")
               (:coalton-file "src/ssz")
               (:file "src/ssz-wrappers")
               (:file "test/package")
               (:file "test/ssz-basic")
               (:file "test/ssz-generic")))
