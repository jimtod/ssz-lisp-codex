(asdf:defsystem #:ssz-lisp
  :description "SSZ (Simple Serialize) for Ethereum in Coalton"
  :author "rsl"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:coalton #:fiveam #:ironclad #:cl-yaml)
  :serial t
  :components ((:file "src/package")
               (:file "src/ssz")
               (:file "test/package")
               (:file "test/ssz-basic")
               (:file "test/ssz-generic")))
