(package ssz
  (import
    coalton-prelude)
  (import-from
    coalton-library/lisparray
    LispArray)
  (import-from
    coalton-library/math/integral
    div)
  (export
    ByteArray
    SSZType
    SSZValue
    make-uint-type
    make-bool-type
    make-byte-type
    make-bytesn-type
    make-bitvector-type
    make-bitlist-type
    make-progressive-list-type
    make-progressive-bitlist-type
    make-none-type
    make-union-type
    make-compatible-union-type
    make-compatible-union-type-from
    make-vector-type
    make-list-type
    make-container-type
    make-progressive-container-type
    make-vuint
    make-vbool
    make-vbyte
    make-vbytes
    make-vbitvector
    make-vbitlist
    vunion-none
    vunion
    make-vvector
    make-vlist
    make-vcontainer
    encode
    decode
    hash-tree-root
    byte-array-length
    byte-array-ref))

(define-type-alias ByteArray (LispArray U8))

(define-type SSZType
  (UintType U32)
  (BoolType)
  (ByteType)
  (BytesNType U32)
  (BitVectorType U32)
  (BitListType U32)
  (ProgressiveListType SSZType)
  (ProgressiveBitlistType)
  (NoneType)
  (UnionType (List SSZType))
  (CompatibleUnionType (List (Tuple U8 SSZType)))
  (VectorType SSZType U32)
  (ListType SSZType U32)
  (ContainerType (List SSZType))
  (ProgressiveContainerType (List SSZType) (List Boolean)))

(define-type SSZValue
  (VUInt Integer)
  (VBool Boolean)
  (VByte U8)
  (VBytes ByteArray)
  (VBitVector (List Boolean))
  (VBitList (List Boolean))
  (VUnionNone U8)
  (VUnionSome U8 SSZValue)
  (VVector (List SSZValue))
  (VList (List SSZValue))
  (VContainer (List SSZValue)))


(declare neq (Eq :a => :a -> :a -> Boolean))
(define (neq a b)
  (not (== a b)))

(declare make-uint-type (U32 -> SSZType))
(define (make-uint-type n-bits)
  (UintType n-bits))

(declare make-bool-type SSZType)
(define make-bool-type
  BoolType)

(declare make-byte-type SSZType)
(define make-byte-type
  ByteType)

(declare make-bytesn-type (U32 -> SSZType))
(define (make-bytesn-type n)
  (BytesNType n))

(declare make-bitvector-type (U32 -> SSZType))
(define (make-bitvector-type n)
  (BitVectorType n))

(declare make-bitlist-type (U32 -> SSZType))
(define (make-bitlist-type max-n)
  (BitListType max-n))

(declare make-progressive-list-type (SSZType -> SSZType))
(define (make-progressive-list-type elem)
  (ProgressiveListType elem))

(declare make-progressive-bitlist-type SSZType)
(define make-progressive-bitlist-type
  ProgressiveBitlistType)

(declare make-none-type SSZType)
(define make-none-type
  NoneType)

(declare make-union-type ((List SSZType) -> SSZType))
(define (make-union-type types)
  (UnionType types))

(declare make-compatible-union-type ((List (Tuple U8 SSZType)) -> SSZType))
(define (make-compatible-union-type selectors)
  (CompatibleUnionType selectors))

(declare make-compatible-union-type-from ((List U8) -> (List SSZType) -> SSZType))
(define (make-compatible-union-type-from selectors types)
  (CompatibleUnionType (zip selectors types)))

(declare make-vector-type (SSZType -> U32 -> SSZType))
(define (make-vector-type elem len)
  (VectorType elem len))

(declare make-list-type (SSZType -> U32 -> SSZType))
(define (make-list-type elem max-len)
  (ListType elem max-len))

(declare make-container-type ((List SSZType) -> SSZType))
(define (make-container-type fields)
  (ContainerType fields))

(declare make-progressive-container-type ((List SSZType) -> (List Boolean) -> SSZType))
(define (make-progressive-container-type fields active-fields)
  (let ((field-count (list-length fields))
        (active-count (list-length active-fields))
        (enabled-count
          (fold (fn (acc b) (if b (+ acc 1) acc)) 0 active-fields)))
    (if (== field-count 0)
        (lisp SSZType () (cl:error "progressive container requires at least one field"))
        (if (== active-count 0)
            (lisp SSZType () (cl:error "active_fields must not be empty"))
            (if (> active-count 256)
                (lisp SSZType (active-count) (cl:error "active_fields length exceeds 256"))
                (if (not (list-last active-fields))
                    (lisp SSZType () (cl:error "active_fields must end with 1"))
                    (if (neq enabled-count field-count)
                        (lisp SSZType (enabled-count field-count)
                          (cl:error "active_fields count must match field count"))
                        (ProgressiveContainerType fields active-fields))))))))

(declare make-vuint (Integer -> SSZValue))
(define (make-vuint n)
  (VUInt n))

(declare make-vbool (Boolean -> SSZValue))
(define (make-vbool b)
  (VBool b))

(declare make-vbyte (U8 -> SSZValue))
(define (make-vbyte b)
  (VByte b))

(declare make-vbytes (ByteArray -> SSZValue))
(define (make-vbytes bytes)
  (VBytes bytes))

(declare make-vbitvector ((List Boolean) -> SSZValue))
(define (make-vbitvector bits)
  (VBitVector bits))

(declare make-vbitlist ((List Boolean) -> SSZValue))
(define (make-vbitlist bits)
  (VBitList bits))

(declare vunion-none (U8 -> SSZValue))
(define (vunion-none selector)
  (VUnionNone selector))

(declare vunion (U8 -> SSZValue -> SSZValue))
(define (vunion selector val)
  (VUnionSome selector val))

(declare make-vvector ((List SSZValue) -> SSZValue))
(define (make-vvector vals)
  (VVector vals))

(declare make-vlist ((List SSZValue) -> SSZValue))
(define (make-vlist vals)
  (VList vals))

(declare make-vcontainer ((List SSZValue) -> SSZValue))
(define (make-vcontainer vals)
  (VContainer vals))

(declare byte-array-length (ByteArray -> U32))
(define (byte-array-length arr)
  (lisp U32 (arr) (cl:length arr)))

(declare byte-array-ref (ByteArray -> U32 -> U8))
(define (byte-array-ref arr idx)
  (lisp U8 (arr idx) (cl:aref arr idx)))

(declare list-length ((List :a) -> U32))
(define (list-length lst)
  (lisp U32 (lst) (cl:length lst)))

(declare list-nth ((List :a) -> U32 -> :a))
(define (list-nth lst idx)
  (lisp :a (lst idx) (cl:nth idx lst)))

(declare list-last ((List :a) -> :a))
(define (list-last lst)
  (lisp :a (lst)
    (cl:let ((last-cell (cl:last lst)))
      (cl:if last-cell
             (cl:car last-cell)
             (cl:error "empty list")))))

(declare range-u32 (U32 -> U32 -> (List U32)))
(define (range-u32 start end)
  (if (>= start end)
      Nil
      (cons start (range-u32 (+ start 1) end))))

(declare validate-offsets ((List U32) -> U32 -> U32 -> Boolean))
(define (validate-offsets offsets total min-offset)
  (if (> min-offset total)
      False
      (match
       (fold
        (fn (st off)
          (match st
            ((Tuple is-valid prev)
             (if is-valid
                 (let ((aligned (== (mod off 4) 0))
                       (in-range (and (>= off min-offset) (<= off total)))
                       (monotonic (>= off prev)))
                   (Tuple (and aligned in-range monotonic) off))
                 (Tuple False prev)))))
        (Tuple True min-offset)
        offsets)
       ((Tuple is-valid _ignored) is-valid))))

(declare bytes-concat ((List ByteArray) -> ByteArray))
(define (bytes-concat chunks)
  (lisp ByteArray (chunks) (ssz-helpers::%concat-bytes chunks)))

(declare bytes-slice (ByteArray -> U32 -> U32 -> ByteArray))
(define (bytes-slice bytes start end)
  (lisp ByteArray (bytes start end) (ssz-helpers::%slice-bytes bytes start end)))

(declare u32-to-bytes (U32 -> ByteArray))
(define (u32-to-bytes value)
  (lisp ByteArray (value) (ssz-helpers::%u32-to-bytes value)))

(declare u32-from-bytes (ByteArray -> U32 -> U32))
(define (u32-from-bytes bytes start)
  (lisp U32 (bytes start) (ssz-helpers::%u32-from-bytes bytes start)))

(declare u8-to-u32 (U8 -> U32))
(define (u8-to-u32 value)
  (lisp U32 (value) value))

(declare bytes-zero32 ByteArray)
(define bytes-zero32
  (lisp ByteArray () (ssz-helpers::%zero-bytes 32)))

(declare fixed-size (SSZType -> (Tuple Boolean U32)))
(define (fixed-size t)
  (match t
    ((UintType n) (Tuple True (div n 8)))
    ((BoolType) (Tuple True 1))
    ((ByteType) (Tuple True 1))
    ((BytesNType n) (Tuple True n))
    ((BitVectorType n)
     (let ((byte-len (lisp U32 (n) (cl:ceiling n 8))))
       (Tuple True byte-len)))
    ((BitListType _ignored)
     (Tuple False 0))
    ((ProgressiveListType _ignored) (Tuple False 0))
    ((ProgressiveBitlistType) (Tuple False 0))
    ((NoneType) (Tuple True 0))
    ((UnionType _ignored) (Tuple False 0))
    ((CompatibleUnionType _ignored) (Tuple False 0))
    ((VectorType elem len)
     (match (fixed-size elem)
       ((Tuple is-fixed elem-size)
        (if is-fixed
            (Tuple True (* elem-size len))
            (Tuple False 0)))))
    ((ListType _ignored-elem _ignored-max) (Tuple False 0))
    ((ContainerType fields)
     (let ((acc (Tuple True 0)))
       (fold
        (fn (state field)
          (match state
            ((Tuple is-fixed total)
             (if is-fixed
                 (match (fixed-size field)
                   ((Tuple field-is-fixed field-size)
                    (if field-is-fixed
                        (Tuple True (+ total field-size))
                        (Tuple False 0))))
                 (Tuple False 0)))))
        acc
        fields)))
    ((ProgressiveContainerType fields _ignored)
     (fixed-size (ContainerType fields)))))

(declare is-basic-type (SSZType -> Boolean))
(define (is-basic-type t)
  (match t
    ((UintType _ignored) True)
    ((BoolType) True)
    ((ByteType) True)
    (_ignored False)))

(declare size-of-basic (SSZType -> U32))
(define (size-of-basic t)
  (match t
    ((UintType n) (div n 8))
    ((BoolType) 1)
    ((ByteType) 1)
    (_ignored (lisp U32 () (cl:error "not a basic type")))))

(declare chunk-count (SSZType -> U32))
(define (chunk-count t)
  (match t
    ((UintType _ignored) 1)
    ((BoolType) 1)
    ((ByteType) 1)
    ((BytesNType n)
     (lisp U32 (n) (ssz-helpers::%ceil-div n 32)))
    ((BitVectorType n)
     (lisp U32 (n) (ssz-helpers::%ceil-div (cl:+ n 255) 256)))
    ((BitListType max-n)
     (lisp U32 (max-n) (ssz-helpers::%ceil-div (cl:+ max-n 255) 256)))
    ((ProgressiveBitlistType) 1)
    ((VectorType elem len)
     (if (is-basic-type elem)
         (lisp U32 (len elem)
           (ssz-helpers::%ceil-div (cl:* len (size-of-basic elem)) 32))
         len))
    ((ListType elem max-len)
     (if (is-basic-type elem)
         (lisp U32 (max-len elem)
           (ssz-helpers::%ceil-div (cl:* max-len (size-of-basic elem)) 32))
         max-len))
    ((ProgressiveListType _ignored) 1)
    ((ContainerType fields) (list-length fields))
    ((ProgressiveContainerType fields _ignored) (list-length fields))
    ((CompatibleUnionType _ignored) 1)
    (_ignored 1)))

(declare bytes-to-chunks (ByteArray -> (List ByteArray)))
(define (bytes-to-chunks bytes)
  (lisp (List ByteArray) (bytes) (ssz-helpers::%bytes-to-chunks bytes)))

(declare merkleize ((List ByteArray) -> ByteArray))
(define (merkleize chunks)
  (lisp ByteArray (chunks) (ssz-helpers::%merkleize chunks nil)))

(declare merkleize-limit ((List ByteArray) -> U32 -> ByteArray))
(define (merkleize-limit chunks limit)
  (lisp ByteArray (chunks limit) (ssz-helpers::%merkleize chunks limit)))

(declare merkleize-progressive ((List ByteArray) -> ByteArray))
(define (merkleize-progressive chunks)
  (lisp ByteArray (chunks) (ssz-helpers::%merkleize-progressive chunks 1)))

(declare mix-in-length (ByteArray -> U32 -> ByteArray))
(define (mix-in-length root len)
  (lisp ByteArray (root len) (ssz-helpers::%mix-in-length root len)))

(declare mix-in-selector (ByteArray -> U8 -> ByteArray))
(define (mix-in-selector root selector)
  (lisp ByteArray (root selector) (ssz-helpers::%mix-in-selector root selector)))

(declare mix-in-active-fields (ByteArray -> (List Boolean) -> ByteArray))
(define (mix-in-active-fields root active-fields)
  (let ((packed (pack-bits active-fields)))
    (lisp ByteArray (root packed) (ssz-helpers::%mix-in-active-fields root packed))))

(declare pack-bits ((List Boolean) -> ByteArray))
(define (pack-bits bits)
  (let ((count (list-length bits)))
    (lisp ByteArray (bits count) (ssz-helpers::%pack-bits bits count))))

(declare encode-uint (U32 -> Integer -> ByteArray))
(define (encode-uint n-bits value)
  (lisp ByteArray (n-bits value)
    (ssz-helpers::%encode-uint n-bits value)))

(declare encode-bool (Boolean -> ByteArray))
(define (encode-bool value)
  (lisp ByteArray (value)
    (cl:let ((arr (cl:make-array 1 :element-type '(cl:unsigned-byte 8))))
      (cl:setf (cl:aref arr 0) (cl:if value 1 0))
      arr)))

(declare encode-byte (U8 -> ByteArray))
(define (encode-byte value)
  (lisp ByteArray (value)
    (cl:let ((arr (cl:make-array 1 :element-type '(cl:unsigned-byte 8))))
      (cl:setf (cl:aref arr 0) value)
      arr)))

(declare encode-bytesn (U32 -> ByteArray -> ByteArray))
(define (encode-bytesn n bytes)
  (lisp ByteArray (n bytes)
    (cl:let ((len (cl:length bytes)))
      (cl:unless (cl:= len n)
        (cl:error "bytesN length mismatch: expected ~a, got ~a" n len))
      bytes)))

(declare encode-bitvector (U32 -> (List Boolean) -> ByteArray))
(define (encode-bitvector n bits)
  (let ((count (list-length bits)))
    (if (neq count n)
        (lisp ByteArray (count n) (cl:error "bitvector length mismatch: expected ~a, got ~a" n count))
        (lisp ByteArray (bits n) (ssz-helpers::%pack-bits bits n)))))

(declare encode-bitlist (U32 -> (List Boolean) -> ByteArray))
(define (encode-bitlist max-n bits)
  (let ((count (list-length bits)))
    (if (> count max-n)
        (lisp ByteArray (count max-n) (cl:error "bitlist length exceeds max: ~a > ~a" count max-n))
        (lisp ByteArray (bits) (ssz-helpers::%pack-bitlist bits)))))

(declare encode-progressive-list ((SSZValue -> SSZType -> ByteArray) -> SSZType -> (List SSZValue) -> ByteArray))
(define (encode-progressive-list encode-f elem vals)
  (match (fixed-size elem)
    ((Tuple is-fixed _ignored)
     (if is-fixed
         (bytes-concat (map (fn (v) (encode-f v elem)) vals))
         (let ((count (list-length vals))
               (encs (map (fn (v) (encode-f v elem)) vals)))
           (let ((fixed-size-bytes (* count 4)))
             (let ((offsets
                     (match (fold
                             (fn (state enc)
                               (match state
                                 ((Tuple current offs)
                                  (Tuple (+ current (byte-array-length enc))
                                         (cons current offs)))))
                             (Tuple fixed-size-bytes Nil)
                             encs)
                       ((Tuple _ignored offs) offs))))
               (let ((offset-bytes (map (fn (o) (u32-to-bytes o)) (reverse offsets))))
                 (bytes-concat (append offset-bytes encs))))))))))

(declare encode-progressive-bitlist ((List Boolean) -> ByteArray))
(define (encode-progressive-bitlist bits)
  (lisp ByteArray (bits) (ssz-helpers::%pack-bitlist bits)))

(declare encode-union ((SSZValue -> SSZType -> ByteArray) -> (List SSZType) -> U8 -> SSZValue -> ByteArray))
(define (encode-union encode-f types selector val)
  (let ((index (u8-to-u32 selector))
         (type-count (list-length types)))
    (if (>= index type-count)
        (lisp ByteArray (index type-count) (cl:error "union selector out of bounds"))
        (let ((selected (list-nth types index)))
          (match selected
            ((NoneType)
             (match val
               ((VUnionNone sel)
                (if (neq sel selector)
                    (lisp ByteArray (sel selector) (cl:error "union selector mismatch"))
                    (if (neq selector 0)
                        (lisp ByteArray (selector) (cl:error "union none requires selector 0"))
                        (encode-byte selector))))
               (_ignored (lisp ByteArray () (cl:error "union none requires VUnionNone")))))
            (_ignored
             (match val
               ((VUnionSome sel inner)
                (if (neq sel selector)
                    (lisp ByteArray (sel selector) (cl:error "union selector mismatch"))
                    (bytes-concat (Cons (encode-byte selector)
                                        (Cons (encode-f inner selected) Nil)))))
               (_ignored (lisp ByteArray () (cl:error "union value requires VUnionSome"))))))))))

(declare find-compatible ((List (Tuple U8 SSZType)) -> U8 -> SSZType))
(define (find-compatible selectors selector)
  (match selectors
    ((Cons pair rest)
     (match pair
       ((Tuple sel t)
        (if (== sel selector)
            t
            (find-compatible rest selector)))))
    ((Nil) (lisp SSZType (selector) (cl:error "compatible union selector not found")))))

(declare encode-compatible-union ((SSZValue -> SSZType -> ByteArray) -> (List (Tuple U8 SSZType)) -> SSZValue -> ByteArray))
(define (encode-compatible-union encode-f selectors val)
  (match val
    ((VUnionNone selector)
     (let ((selected (find-compatible selectors selector)))
       (match selected
         ((NoneType) (encode-byte selector))
         (_ignored (lisp ByteArray () (cl:error "compatible union none requires NoneType"))))))
    ((VUnionSome selector inner)
     (let ((selected (find-compatible selectors selector)))
       (match selected
         ((NoneType) (lisp ByteArray () (cl:error "compatible union none selected with value")))
         (_ignored (bytes-concat (Cons (encode-byte selector)
                                      (Cons (encode-f inner selected) Nil)))))))
    (_ignored (lisp ByteArray () (cl:error "compatible union requires union value")))))

(declare encode-vector ((SSZValue -> SSZType -> ByteArray) -> SSZType -> U32 -> (List SSZValue) -> ByteArray))
(define (encode-vector encode-f elem len vals)
  (let ((count (list-length vals)))
    (if (neq count len)
        (lisp ByteArray (count len) (cl:error "vector length mismatch: expected ~a, got ~a" len count))
        (match (fixed-size elem)
          ((Tuple is-fixed _ignored)
           (if is-fixed
               (bytes-concat (map (fn (v) (encode-f v elem)) vals))
               (let ((fixed-size-bytes (* len 4))
                     (encs (map (fn (v) (encode-f v elem)) vals)))
                 (let ((offsets
                         (match (fold
                                 (fn (state enc)
                                   (match state
                                     ((Tuple current offs)
                                      (Tuple (+ current (byte-array-length enc))
                                             (cons current offs)))))
                                 (Tuple fixed-size-bytes Nil)
                                 encs)
                           ((Tuple _ignored offs) offs))))
                   (let ((offset-bytes (map (fn (o) (u32-to-bytes o)) (reverse offsets))))
                     (bytes-concat (append offset-bytes encs)))))))))))

(declare encode-list ((SSZValue -> SSZType -> ByteArray) -> SSZType -> U32 -> (List SSZValue) -> ByteArray))
(define (encode-list encode-f elem max-len vals)
  (let ((count (list-length vals)))
    (if (> count max-len)
        (lisp ByteArray (count max-len) (cl:error "list length exceeds max: ~a > ~a" count max-len))
        (match (fixed-size elem)
          ((Tuple is-fixed _ignored)
           (if is-fixed
               (bytes-concat (map (fn (v) (encode-f v elem)) vals))
               (let ((fixed-size-bytes (* count 4))
                     (encs (map (fn (v) (encode-f v elem)) vals)))
                 (let ((offsets
                         (match (fold
                                 (fn (state enc)
                                   (match state
                                     ((Tuple current offs)
                                      (Tuple (+ current (byte-array-length enc))
                                             (cons current offs)))))
                                 (Tuple fixed-size-bytes Nil)
                                 encs)
                           ((Tuple _ignored offs) offs))))
                   (let ((offset-bytes (map (fn (o) (u32-to-bytes o)) (reverse offsets))))
                     (bytes-concat (append offset-bytes encs)))))))))))

(declare encode-container ((SSZValue -> SSZType -> ByteArray) -> (List SSZType) -> (List SSZValue) -> ByteArray))
(define (encode-container encode-f fields vals)
  (let ((field-count (list-length fields))
        (val-count (list-length vals)))
    (if (neq field-count val-count)
        (lisp ByteArray (field-count val-count) (cl:error "container field/value mismatch: ~a vs ~a" field-count val-count))
        (match (fixed-size (ContainerType fields))
          ((Tuple is-fixed _ignored)
           (if is-fixed
               (bytes-concat
                (map (fn (pair)
                       (match pair
                         ((Tuple val field) (encode-f val field))))
                     (zip vals fields)))
               (let ((fixed-size-bytes
                       (fold
                        (fn (acc field)
                          (match (fixed-size field)
                            ((Tuple field-ok field-size)
                             (if field-ok
                                 (+ acc field-size)
                                 (+ acc 4)))))
                        0
                        fields)))
                 (let ((pairs (zip vals fields)))
                   (match
                    (fold
                     (fn (st pair)
                       (match st
                         ((Tuple current (Tuple fixed-chunks var-chunks))
                          (match pair
                            ((Tuple val field)
                             (match (fixed-size field)
                               ((Tuple field-ok _ignored)
                                (if field-ok
                                    (Tuple current
                                           (Tuple (cons (encode-f val field) fixed-chunks)
                                                  var-chunks))
                                    (let ((enc (encode-f val field)))
                                      (Tuple (+ current (byte-array-length enc))
                                             (Tuple (cons (u32-to-bytes current) fixed-chunks)
                                                    (cons enc var-chunks))))))))))))
                 (Tuple fixed-size-bytes (Tuple Nil Nil))
                 pairs)
                    ((Tuple _ignored (Tuple fixed-chunks var-chunks))
                     (bytes-concat (append (reverse fixed-chunks) (reverse var-chunks)))))))))))))

(declare decode-uint (U32 -> ByteArray -> Integer))
(define (decode-uint n-bits bytes)
  (lisp Integer (n-bits bytes)
    (cl:let ((byte-len (cl:truncate n-bits 8))
             (len (cl:length bytes)))
      (cl:unless (cl:= len byte-len)
        (cl:error "uint byte length mismatch: expected ~a, got ~a" byte-len len))
      (cl:let ((acc 0))
        (cl:dotimes (i byte-len acc)
          (cl:setf acc (cl:+ acc (cl:ash (cl:aref bytes i) (cl:* 8 i)))))))))

(declare decode-bool (ByteArray -> Boolean))
(define (decode-bool bytes)
  (lisp Boolean (bytes)
    (cl:let ((len (cl:length bytes)))
      (cl:unless (cl:= len 1)
        (cl:error "bool byte length mismatch: expected 1, got ~a" len))
      (cl:let ((b (cl:aref bytes 0)))
        (cl:cond ((cl:= b 0) nil)
                 ((cl:= b 1) cl:t)
                 (cl:t (cl:error "invalid bool value: ~a" b)))))))

(declare decode-byte (ByteArray -> U8))
(define (decode-byte bytes)
  (lisp U8 (bytes)
    (cl:let ((len (cl:length bytes)))
      (cl:unless (cl:= len 1)
        (cl:error "byte length mismatch: expected 1, got ~a" len))
      (cl:aref bytes 0))))

(declare decode-bytesn (U32 -> ByteArray -> ByteArray))
(define (decode-bytesn n bytes)
  (lisp ByteArray (n bytes)
    (cl:let ((len (cl:length bytes)))
      (cl:unless (cl:= len n)
        (cl:error "bytesN length mismatch: expected ~a, got ~a" n len))
      bytes)))

(declare decode-bitvector (U32 -> ByteArray -> (List Boolean)))
(define (decode-bitvector n bytes)
  (lisp (List Boolean) (bytes n)
    (cl:progn
      (ssz-helpers::%validate-bitvector-tail bytes n)
      (ssz-helpers::%unpack-bitvector bytes n))))

(declare decode-bitlist (U32 -> ByteArray -> (List Boolean)))
(define (decode-bitlist max-n bytes)
  (lisp (List Boolean) (bytes max-n)
    (ssz-helpers::%unpack-bitlist bytes max-n)))

(declare decode-progressive-list ((ByteArray -> SSZType -> SSZValue) -> SSZType -> ByteArray -> (List SSZValue)))
(define (decode-progressive-list decode-f elem bytes)
  (match (fixed-size elem)
    ((Tuple is-fixed elem-size)
     (if is-fixed
         (let ((total (byte-array-length bytes)))
           (if (== elem-size 0)
               Nil
               (if (neq (mod total elem-size) 0)
                   (lisp (List SSZValue) (total elem-size) (cl:error "progressive list byte length mismatch"))
                   (let ((count (div total elem-size)))
                     (map (fn (i)
                            (decode-f (bytes-slice bytes (* i elem-size) (* (+ i 1) elem-size)) elem))
                          (range-u32 0 count))))))
         (let ((total (byte-array-length bytes)))
           (if (== total 0)
               Nil
               (let ((first-offset (u32-from-bytes bytes 0)))
                 (if (neq (mod first-offset 4) 0)
                     (lisp (List SSZValue) (first-offset) (cl:error "invalid progressive list offset"))
                     (let ((count (div first-offset 4)))
                       (let ((offsets
                               (map (fn (i) (u32-from-bytes bytes (* i 4)))
                                    (range-u32 0 count))))
                         (if (not (validate-offsets offsets total first-offset))
                             (lisp (List SSZValue) (offsets) (cl:error "invalid progressive list offsets"))
                             (map (fn (i)
                                    (let ((start (list-nth offsets i))
                                          (end (if (== (+ i 1) count)
                                                   total
                                                   (list-nth offsets (+ i 1)))))
                                      (decode-f (bytes-slice bytes start end) elem)))
                                  (range-u32 0 count)))))))))))))

(declare decode-progressive-bitlist (ByteArray -> (List Boolean)))
(define (decode-progressive-bitlist bytes)
  (let ((max-bits (* (byte-array-length bytes) 8)))
    (lisp (List Boolean) (bytes max-bits)
      (ssz-helpers::%unpack-bitlist bytes max-bits))))

(declare decode-union ((ByteArray -> SSZType -> SSZValue) -> (List SSZType) -> ByteArray -> SSZValue))
(define (decode-union decode-f types bytes)
  (let ((total (byte-array-length bytes)))
    (if (< total 1)
        (lisp SSZValue () (cl:error "union requires selector byte"))
        (let ((selector (byte-array-ref bytes 0))
               (index (u8-to-u32 selector))
               (type-count (list-length types)))
          (if (>= index type-count)
              (lisp SSZValue (index type-count) (cl:error "union selector out of bounds"))
              (let ((selected (list-nth types index)))
                (match selected
                  ((NoneType)
                   (if (neq selector 0)
                       (lisp SSZValue (selector) (cl:error "union none requires selector 0"))
                       (if (neq total 1)
                        (lisp SSZValue (total) (cl:error "union none must be selector only"))
                        (VUnionNone selector))))
                  (_ignored
                   (let ((payload (bytes-slice bytes 1 total)))
                     (VUnionSome selector (decode-f payload selected)))))))))))

(declare decode-compatible-union ((ByteArray -> SSZType -> SSZValue) -> (List (Tuple U8 SSZType)) -> ByteArray -> SSZValue))
(define (decode-compatible-union decode-f selectors bytes)
  (let ((total (byte-array-length bytes)))
    (if (< total 1)
        (lisp SSZValue () (cl:error "compatible union requires selector byte"))
        (let ((selector (byte-array-ref bytes 0)))
          (let ((selected (find-compatible selectors selector)))
            (match selected
              ((NoneType)
               (if (neq total 1)
                   (lisp SSZValue (total) (cl:error "compatible union none must be selector only"))
                   (VUnionNone selector)))
              (_ignored
               (let ((payload (bytes-slice bytes 1 total)))
                 (VUnionSome selector (decode-f payload selected))))))))))

(declare decode-vector ((ByteArray -> SSZType -> SSZValue) -> SSZType -> U32 -> ByteArray -> (List SSZValue)))
(define (decode-vector decode-f elem len bytes)
  (match (fixed-size elem)
    ((Tuple is-fixed elem-size)
     (if is-fixed
         (let ((total (byte-array-length bytes)))
           (if (neq total (* len elem-size))
               (lisp (List SSZValue) (total len elem-size) (cl:error "vector byte length mismatch"))
               (map (fn (i)
                      (decode-f (bytes-slice bytes (* i elem-size) (* (+ i 1) elem-size)) elem))
                    (range-u32 0 len))))
         (let ((total (byte-array-length bytes)))
           (if (== total 0)
               (if (== len 0)
                   Nil
                   (lisp (List SSZValue) (len) (cl:error "empty vector encoding")))
               (let ((first-offset (u32-from-bytes bytes 0)))
                 (if (neq (mod first-offset 4) 0)
                     (lisp (List SSZValue) (first-offset) (cl:error "invalid vector offset"))
                     (let ((count (div first-offset 4)))
                       (if (neq count len)
                           (lisp (List SSZValue) (count len) (cl:error "vector offset count mismatch"))
                           (let ((offsets
                                   (map (fn (i) (u32-from-bytes bytes (* i 4)))
                                        (range-u32 0 len))))
                             (if (not (validate-offsets offsets total (* len 4)))
                                 (lisp (List SSZValue) (offsets) (cl:error "invalid vector offsets"))
                                 (map (fn (i)
                                        (let ((start (list-nth offsets i))
                                              (end (if (== (+ i 1) len)
                                                       total
                                                       (list-nth offsets (+ i 1)))))
                                          (decode-f (bytes-slice bytes start end) elem)))
                                      (range-u32 0 len))))))))))))))

(declare decode-list ((ByteArray -> SSZType -> SSZValue) -> SSZType -> U32 -> ByteArray -> (List SSZValue)))
(define (decode-list decode-f elem max-len bytes)
  (match (fixed-size elem)
    ((Tuple is-fixed elem-size)
     (if is-fixed
         (let ((total (byte-array-length bytes)))
           (if (neq (mod total elem-size) 0)
               (lisp (List SSZValue) (total elem-size) (cl:error "list byte length mismatch"))
               (let ((count (div total elem-size)))
                 (if (> count max-len)
                     (lisp (List SSZValue) (count max-len) (cl:error "list length exceeds max"))
                     (map (fn (i)
                            (decode-f (bytes-slice bytes (* i elem-size) (* (+ i 1) elem-size)) elem))
                          (range-u32 0 count))))))
         (let ((total (byte-array-length bytes)))
           (if (== total 0)
               Nil
               (let ((first-offset (u32-from-bytes bytes 0)))
                 (if (neq (mod first-offset 4) 0)
                     (lisp (List SSZValue) (first-offset) (cl:error "invalid list offset"))
                     (let ((count (div first-offset 4)))
                       (if (> count max-len)
                           (lisp (List SSZValue) (count max-len) (cl:error "list length exceeds max"))
                           (let ((offsets
                                   (map (fn (i) (u32-from-bytes bytes (* i 4)))
                                        (range-u32 0 count))))
                             (if (not (validate-offsets offsets total first-offset))
                                 (lisp (List SSZValue) (offsets) (cl:error "invalid list offsets"))
                                 (map (fn (i)
                                        (let ((start (list-nth offsets i))
                                              (end (if (== (+ i 1) count)
                                                       total
                                                       (list-nth offsets (+ i 1)))))
                                          (decode-f (bytes-slice bytes start end) elem)))
                                      (range-u32 0 count))))))))))))))

(declare container-offsets ((List SSZType) -> ByteArray -> (List U32)))
(define (container-offsets fields bytes)
  (let ((state
          (fold
           (fn (st field)
             (match st
               ((Tuple pos offs)
                (match (fixed-size field)
                  ((Tuple is-fixed size)
                   (if is-fixed
                       (Tuple (+ pos size) offs)
                       (Tuple (+ pos 4) (cons (u32-from-bytes bytes pos) offs))))))))
           (Tuple 0 Nil)
           fields)))
    (match state
      ((Tuple _ignored offs) (reverse offs)))))

(declare container-fixed-size ((List SSZType) -> U32))
(define (container-fixed-size fields)
  (fold
   (fn (acc field)
     (match (fixed-size field)
       ((Tuple is-fixed size)
        (if is-fixed
            (+ acc size)
            (+ acc 4)))))
   0
   fields))

(declare decode-container ((ByteArray -> SSZType -> SSZValue) -> (List SSZType) -> ByteArray -> (List SSZValue)))
(define (decode-container decode-f fields bytes)
  (match (fixed-size (ContainerType fields))
    ((Tuple is-fixed _ignored)
     (if is-fixed
         (match
          (fold
           (fn (st field)
             (match st
               ((Tuple pos acc)
                (match (fixed-size field)
                  ((Tuple field-is-fixed size)
                   (if field-is-fixed
                       (Tuple (+ pos size)
                              (cons (decode-f (bytes-slice bytes pos (+ pos size)) field) acc))
                       (lisp (Tuple U32 (List SSZValue)) (field)
                         (cl:error "unexpected variable field"))))))))
           (Tuple 0 Nil)
           fields)
          ((Tuple _ignored acc) (reverse acc)))
         (let ((total (byte-array-length bytes))
               (fixed-size-bytes (container-fixed-size fields))
               (offsets (container-offsets fields bytes)))
           (if (not (validate-offsets offsets total fixed-size-bytes))
               (lisp (List SSZValue) (offsets) (cl:error "invalid container offsets"))
               (match
                (fold
                 (fn (st field)
                   (match st
                     ((Tuple pos (Tuple idx acc))
                      (match (fixed-size field)
                        ((Tuple field-is-fixed size)
                         (if field-is-fixed
                             (Tuple (+ pos size)
                                    (Tuple idx
                                           (cons (decode-f (bytes-slice bytes pos (+ pos size)) field) acc)))
                             (let ((start (list-nth offsets idx))
                                   (end (if (== (+ idx 1) (list-length offsets))
                                            total
                                            (list-nth offsets (+ idx 1)))))
                               (Tuple (+ pos 4)
                                      (Tuple (+ idx 1)
                                             (cons (decode-f (bytes-slice bytes start end) field) acc))))))))))
                 (Tuple 0 (Tuple 0 Nil))
                 fields)
                ((Tuple _ignored-first (Tuple _ignored-second acc)) (reverse acc)))))))))

(declare hash-tree-root (SSZValue -> SSZType -> ByteArray))
(define (hash-tree-root value t)
  (match (Tuple value t)
    ((Tuple (VUInt _ignored-val) (UintType _ignored-bits))
     (merkleize (bytes-to-chunks (encode value t))))
    ((Tuple (VBool _ignored) (BoolType))
     (merkleize (bytes-to-chunks (encode value t))))
    ((Tuple (VByte _ignored) (ByteType))
     (merkleize (bytes-to-chunks (encode value t))))
    ((Tuple (VBytes _ignored-bytes) (BytesNType _ignored-len))
     (merkleize (bytes-to-chunks (encode value t))))
    ((Tuple (VBitVector bits) (BitVectorType n))
     (if (neq (list-length bits) n)
         (lisp ByteArray (n) (cl:error "bitvector length mismatch for hash_tree_root"))
         (merkleize-limit (bytes-to-chunks (pack-bits bits)) (chunk-count t))))
    ((Tuple (VBitList bits) (BitListType max-n))
     (if (> (list-length bits) max-n)
         (lisp ByteArray (max-n) (cl:error "bitlist length exceeds max for hash_tree_root"))
         (mix-in-length
          (merkleize-limit (bytes-to-chunks (pack-bits bits)) (chunk-count t))
          (list-length bits))))
    ((Tuple (VList vals) (ProgressiveListType elem))
     (let ((root
             (if (is-basic-type elem)
                 (merkleize-progressive (bytes-to-chunks (encode value t)))
                 (let ((roots (map (fn (v) (hash-tree-root v elem)) vals)))
                   (merkleize-progressive roots)))))
       (mix-in-length root (list-length vals))))
    ((Tuple (VBitList bits) (ProgressiveBitlistType))
     (mix-in-length
      (merkleize-progressive (bytes-to-chunks (pack-bits bits)))
      (list-length bits)))
    ((Tuple (VVector vals) (VectorType elem _ignored))
     (if (is-basic-type elem)
         (merkleize (bytes-to-chunks (encode value t)))
         (let ((roots (map (fn (v) (hash-tree-root v elem)) vals)))
           (merkleize roots))))
    ((Tuple (VList vals) (ListType elem _ignored))
     (let ((root
             (if (is-basic-type elem)
                 (merkleize-limit (bytes-to-chunks (encode value t)) (chunk-count t))
                 (let ((roots (map (fn (v) (hash-tree-root v elem)) vals)))
                   (merkleize-limit roots (chunk-count t))))))
       (mix-in-length root (list-length vals))))
    ((Tuple (VContainer vals) (ContainerType fields))
     (let ((roots
             (map (fn (pair)
                    (match pair
                      ((Tuple val field) (hash-tree-root val field))))
                  (zip vals fields))))
       (merkleize roots)))
    ((Tuple (VContainer vals) (ProgressiveContainerType fields active-fields))
     (let ((roots
             (map (fn (pair)
                    (match pair
                      ((Tuple val field) (hash-tree-root val field))))
                  (zip vals fields))))
       (mix-in-active-fields (merkleize-progressive roots) active-fields)))
    ((Tuple (VUnionNone selector) (UnionType _types))
     (if (neq selector 0)
         (lisp ByteArray (selector) (cl:error "union none requires selector 0"))
         (mix-in-selector bytes-zero32 selector)))
    ((Tuple (VUnionSome selector val) (UnionType types))
     (let ((index (u8-to-u32 selector))
            (type-count (list-length types)))
       (if (>= index type-count)
           (lisp ByteArray (index type-count) (cl:error "union selector out of bounds"))
           (let ((selected (list-nth types index)))
             (match selected
               ((NoneType) (lisp ByteArray () (cl:error "union none selected with value")))
               (_ignored (mix-in-selector (hash-tree-root val selected) selector)))))))
    ((Tuple (VUnionNone selector) (CompatibleUnionType selectors))
     (let ((selected (find-compatible selectors selector)))
       (match selected
         ((NoneType) (mix-in-selector bytes-zero32 selector))
         (_ignored (lisp ByteArray () (cl:error "compatible union none requires NoneType"))))))
    ((Tuple (VUnionSome selector val) (CompatibleUnionType selectors))
     (let ((selected (find-compatible selectors selector)))
       (match selected
         ((NoneType) (lisp ByteArray () (cl:error "compatible union none selected with value")))
         (_ignored (mix-in-selector (hash-tree-root val selected) selector)))))
    (_ignored (lisp ByteArray () (cl:error "type/value mismatch")))))

(define (encode value t)
  (match (Tuple value t)
    ((Tuple (VUInt v) (UintType n)) (encode-uint n v))
    ((Tuple (VBool v) (BoolType)) (encode-bool v))
    ((Tuple (VByte v) (ByteType)) (encode-byte v))
    ((Tuple (VBytes v) (BytesNType n)) (encode-bytesn n v))
    ((Tuple (VBitVector v) (BitVectorType n)) (encode-bitvector n v))
    ((Tuple (VBitList v) (BitListType max-n)) (encode-bitlist max-n v))
    ((Tuple (VList v) (ProgressiveListType elem)) (encode-progressive-list encode elem v))
    ((Tuple (VBitList v) (ProgressiveBitlistType)) (encode-progressive-bitlist v))
    ((Tuple (VUnionNone sel) (UnionType types)) (encode-union encode types sel value))
    ((Tuple (VUnionSome sel _ignored) (UnionType types)) (encode-union encode types sel value))
    ((Tuple (VUnionNone _ignored) (CompatibleUnionType selectors)) (encode-compatible-union encode selectors value))
    ((Tuple (VUnionSome _ignored _ignored2) (CompatibleUnionType selectors)) (encode-compatible-union encode selectors value))
    ((Tuple (VVector v) (VectorType elem len)) (encode-vector encode elem len v))
    ((Tuple (VList v) (ListType elem max-len)) (encode-list encode elem max-len v))
    ((Tuple (VContainer v) (ContainerType fields)) (encode-container encode fields v))
    ((Tuple (VContainer v) (ProgressiveContainerType fields _ignored)) (encode-container encode fields v))
    (_ignored (lisp ByteArray () (cl:error "type/value mismatch")))))

(define (decode bytes t)
  (match t
    ((UintType n) (VUInt (decode-uint n bytes)))
    ((BoolType) (VBool (decode-bool bytes)))
    ((ByteType) (VByte (decode-byte bytes)))
    ((BytesNType n) (VBytes (decode-bytesn n bytes)))
    ((BitVectorType n) (VBitVector (decode-bitvector n bytes)))
    ((BitListType max-n) (VBitList (decode-bitlist max-n bytes)))
    ((ProgressiveListType elem) (VList (decode-progressive-list decode elem bytes)))
    ((ProgressiveBitlistType) (VBitList (decode-progressive-bitlist bytes)))
    ((NoneType) (VUnionNone 0))
    ((UnionType types) (decode-union decode types bytes))
    ((CompatibleUnionType selectors) (decode-compatible-union decode selectors bytes))
    ((VectorType elem len) (VVector (decode-vector decode elem len bytes)))
    ((ListType elem max-len) (VList (decode-list decode elem max-len bytes)))
    ((ContainerType fields) (VContainer (decode-container decode fields bytes)))
    ((ProgressiveContainerType fields _ignored) (VContainer (decode-container decode fields bytes)))))
