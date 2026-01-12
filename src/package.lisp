(defpackage #:ssz
  (:use #:cl)
  (:export
   #:ssz-type
   #:ssz-value
   #:make-uint-type
   #:make-bool-type
   #:make-byte-type
   #:make-bytesn-type
   #:make-bitvector-type
   #:make-bitlist-type
   #:make-progressive-list-type
   #:make-progressive-bitlist-type
   #:make-none-type
   #:make-union-type
   #:make-vector-type
   #:make-list-type
   #:make-container-type
   #:vuint
   #:vbool
   #:vbyte
   #:vbytes
   #:vbitvector
   #:vbitlist
   #:vunion-none
   #:vunion
   #:vvector
   #:vlist
   #:vcontainer
   #:encode
   #:decode
   #:hash-tree-root
   #:byte-array-length
   #:byte-array-ref))

(defpackage #:ssz.internal
  (:use #:cl))
