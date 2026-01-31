(defpackage #:ssz-test
  (:use #:cl #:fiveam #:ssz))

(declaim (ftype function
                ssz:encode
                ssz:decode
                ssz:hash-tree-root
                ssz::%unpack-bitlist
                ssz::%unpack-bitvector))
