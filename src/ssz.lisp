(in-package #:ssz)

(defun %concat-bytes (chunks)
  (let* ((total (reduce #'+ chunks :key #'length :initial-value 0))
         (out (make-array total :element-type '(unsigned-byte 8))))
    (let ((pos 0))
      (dolist (chunk chunks out)
        (replace out chunk :start1 pos)
        (incf pos (length chunk))))))

(defun %slice-bytes (bytes start end)
  (let* ((len (- end start))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (replace out bytes :start1 0 :start2 start :end2 end)
    out))

(defun %u32-to-bytes (value)
  (let ((arr (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 arr)
      (setf (aref arr i) (ldb (byte 8 (* 8 i)) value)))))

(defun %u32-from-bytes (bytes start)
  (let ((acc 0))
    (dotimes (i 4 acc)
      (setf acc (+ acc (ash (aref bytes (+ start i)) (* 8 i)))))))

(defun %u64-to-32-bytes (value)
  (let ((arr (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i 8 arr)
      (setf (aref arr i) (ldb (byte 8 (* 8 i)) value)))))

(defun %u8-to-32-bytes (value)
  (let ((arr (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref arr 0) value)
    arr))

(defun %zero-bytes (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun %pad-to-32 (bytes)
  (let ((len (length bytes)))
    (cond ((> len 32)
           (error "chunk too large: ~a bytes" len))
          ((= len 32) bytes)
          (t
           (let ((out (%zero-bytes 32)))
             (replace out bytes :start1 0)
             out)))))

(defun %sha256 (bytes)
  (let* ((pkg (find-package :ironclad))
         (sym (and pkg (find-symbol "DIGEST-SEQUENCE" pkg))))
    (if (and sym (fboundp sym))
        (funcall (symbol-function sym) :sha256 bytes)
        (error "ironclad not available; load ironclad to compute SHA256"))))

(defun %hash-pair (left right)
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8))))
    (replace buf left :start1 0)
    (replace buf right :start1 32)
    (%sha256 buf)))

(defun %next-pow-of-two (n)
  (let ((v (max 1 n)))
    (if (zerop (logand v (1- v)))
        v
        (let ((p 1))
          (loop while (< p v) do (setf p (* 2 p)))
          p))))

(defun %merkleize (chunks &optional limit)
  (let* ((len (length chunks))
         (limit-val (and limit (max 0 limit))))
    (when (and limit-val (< limit-val len))
      (error "merkleize limit too small: ~a < ~a" limit-val len))
    (let* ((effective (if limit-val limit-val len))
           (size (%next-pow-of-two effective))
           (pad-count (- size len))
           (layer (if (> pad-count 0)
                      (append chunks
                              (loop repeat pad-count collect (%zero-bytes 32)))
                      (copy-list chunks))))
      (when (= (length layer) 0)
        (setf layer (list (%zero-bytes 32))))
      (loop while (> (length layer) 1) do
        (let ((out nil))
          (loop for (a b) on layer by #'cddr do
            (push (%hash-pair a b) out))
          (setf layer (nreverse out))))
      (car layer))))

(defun %merkleize-progressive (chunks &optional (num-leaves 1))
  (cond ((null chunks) (%zero-bytes 32))
        (t
         (let* ((n (max 1 num-leaves))
                (head (subseq chunks 0 (min n (length chunks))))
                (tail (nthcdr n chunks))
                (left (%merkleize-progressive tail (* n 4)))
                (right (%merkleize head n)))
           (%hash-pair left right)))))

(defun %bytes-to-chunks (bytes)
  (let ((len (length bytes))
        (chunks nil))
    (loop for start from 0 below len by 32 do
      (let ((end (min len (+ start 32))))
        (push (%pad-to-32 (%slice-bytes bytes start end)) chunks)))
    (nreverse chunks)))

(defun %mix-in-length (root length)
  (let* ((len-bytes (%u64-to-32-bytes length))
         (buf (%concat-bytes (list root len-bytes))))
    (%sha256 buf)))

(defun %mix-in-selector (root selector)
  (let* ((sel-bytes (%u8-to-32-bytes selector))
         (buf (%concat-bytes (list root sel-bytes))))
    (%sha256 buf)))

(defun %pack-bits (bits bit-count)
  (let* ((byte-len (ceiling bit-count 8))
         (out (make-array byte-len :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (dotimes (i bit-count out)
      (when (nth i bits)
        (let* ((byte-idx (floor i 8))
               (bit-idx (mod i 8))
               (mask (ash 1 bit-idx)))
          (setf (aref out byte-idx)
                (logior (aref out byte-idx) mask)))))))

(defun %unpack-bitvector (bytes bit-count)
  (let* ((byte-len (ceiling bit-count 8))
         (len (length bytes)))
    (unless (= len byte-len)
      (error "bitvector byte length mismatch: expected ~a, got ~a" byte-len len))
    (let ((bits nil))
      (dotimes (i bit-count (nreverse bits))
        (let* ((byte-idx (floor i 8))
               (bit-idx (mod i 8))
               (b (aref bytes byte-idx))
               (setp (not (zerop (logand b (ash 1 bit-idx))))))
          (push setp bits))))))

(defun %validate-bitvector-tail (bytes bit-count)
  (let* ((byte-len (ceiling bit-count 8))
         (unused (mod (- (* byte-len 8) bit-count) 8)))
    (when (and (> unused 0) (> byte-len 0))
      (let* ((last (aref bytes (- byte-len 1)))
             (mask (1- (ash 1 (- 8 unused)))))
        (when (/= (logand last (lognot mask)) 0)
          (error "bitvector has non-zero unused bits"))))))

(defun %pack-bitlist (bits)
  (let* ((bit-count (length bits))
         (total-bits (+ bit-count 1))
         (byte-len (ceiling total-bits 8))
         (out (make-array byte-len :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (dotimes (i bit-count)
      (when (nth i bits)
        (let* ((byte-idx (floor i 8))
               (bit-idx (mod i 8))
               (mask (ash 1 bit-idx)))
          (setf (aref out byte-idx)
                (logior (aref out byte-idx) mask)))))
    (let* ((delim-idx bit-count)
           (byte-idx (floor delim-idx 8))
           (bit-idx (mod delim-idx 8))
           (mask (ash 1 bit-idx)))
      (setf (aref out byte-idx)
            (logior (aref out byte-idx) mask)))
    out))

(defun %unpack-bitlist (bytes max-bits)
  (let ((len (length bytes)))
    (when (= len 0)
      (error "bitlist must contain a delimiting bit"))
    (let ((highest -1))
      (loop for i from (1- len) downto 0 do
        (let ((b (aref bytes i)))
          (when (> b 0)
            (loop for bit from 7 downto 0 do
              (when (not (zerop (logand b (ash 1 bit))))
                (setf highest (+ (* i 8) bit))
                (return)))
            (when (>= highest 0)
              (return)))))
      (when (< highest 0)
        (error "bitlist missing delimiting bit"))
      (let ((bit-count highest))
        (when (> bit-count max-bits)
          (error "bitlist exceeds max bits: ~a > ~a" bit-count max-bits))
        (let ((bits nil))
          (dotimes (i bit-count (nreverse bits))
            (let* ((byte-idx (floor i 8))
                   (bit-idx (mod i 8))
                   (b (aref bytes byte-idx))
                   (setp (not (zerop (logand b (ash 1 bit-idx))))))
              (push setp bits))))))))

(defun %ceil-div (num den)
  (nth-value 0 (ceiling num den)))

(coalton-toplevel
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
    (VectorType SSZType U32)
    (ListType SSZType U32)
    (ContainerType (List SSZType)))

  (define-type SSZValue
    (VUInt Integer)
    (VBool Boolean)
    (VByte U8)
    (VBytes LispObject)
    (VBitVector (List Boolean))
    (VBitList (List Boolean))
    (VUnionNone U8)
    (VUnionSome U8 SSZValue)
    (VVector (List SSZValue))
    (VList (List SSZValue))
    (VContainer (List SSZValue)))

  (define (make-uint-type (n-bits : U32)) : SSZType
    (UintType n-bits))

  (define (make-bool-type) : SSZType
    (BoolType))

  (define (make-byte-type) : SSZType
    (ByteType))

  (define (make-bytesn-type (n : U32)) : SSZType
    (BytesNType n))

  (define (make-bitvector-type (n : U32)) : SSZType
    (BitVectorType n))

  (define (make-bitlist-type (max-n : U32)) : SSZType
    (BitListType max-n))

  (define (make-progressive-list-type (elem : SSZType)) : SSZType
    (ProgressiveListType elem))

  (define (make-progressive-bitlist-type) : SSZType
    (ProgressiveBitlistType))

  (define (make-none-type) : SSZType
    (NoneType))

  (define (make-union-type (types : (List SSZType))) : SSZType
    (UnionType types))

  (define (make-vector-type (elem : SSZType) (len : U32)) : SSZType
    (VectorType elem len))

  (define (make-list-type (elem : SSZType) (max-len : U32)) : SSZType
    (ListType elem max-len))

  (define (make-container-type (fields : (List SSZType))) : SSZType
    (ContainerType fields))

  (define (vuint (n : Integer)) : SSZValue
    (VUInt n))

  (define (vbool (b : Boolean)) : SSZValue
    (VBool b))

  (define (vbyte (b : U8)) : SSZValue
    (VByte b))

  (define (vbytes (bytes : LispObject)) : SSZValue
    (VBytes bytes))

  (define (vbitvector (bits : (List Boolean))) : SSZValue
    (VBitVector bits))

  (define (vbitlist (bits : (List Boolean))) : SSZValue
    (VBitList bits))

  (define (vunion-none (selector : U8)) : SSZValue
    (VUnionNone selector))

  (define (vunion (selector : U8) (val : SSZValue)) : SSZValue
    (VUnionSome selector val))

  (define (vvector (vals : (List SSZValue))) : SSZValue
    (VVector vals))

  (define (vlist (vals : (List SSZValue))) : SSZValue
    (VList vals))

  (define (vcontainer (vals : (List SSZValue))) : SSZValue
    (VContainer vals))

  (define (byte-array-length (arr : LispObject)) : U32
    (lisp U32 (arr) (length arr)))

  (define (byte-array-ref (arr : LispObject) (idx : U32)) : U8
    (lisp U8 (arr idx) (aref arr idx)))

  (define (list-length (lst : (List a))) : U32
    (lisp U32 (lst) (length lst)))

  (define (list-nth (lst : (List a)) (idx : U32)) : a
    (lisp a (lst idx) (nth idx lst)))

  (define (range-u32 (start : U32) (end : U32)) : (List U32)
    (if (>= start end)
        Nil
        (cons start (range-u32 (+ start 1) end))))

  (define (validate-offsets (offsets : (List U32)) (total : U32) (min-offset : U32)) : Boolean
    (if (> min-offset total)
        False
        (match
         (foldl
          (fn (st off)
            (match st
              ((Tuple ok prev)
               (if ok
                   (let ((aligned (= (mod off 4) 0))
                         (in-range (and (>= off min-offset) (<= off total)))
                         (monotonic (>= off prev)))
                     (Tuple (and aligned in-range monotonic) off))
                   (Tuple False prev)))))
          (Tuple True min-offset)
          offsets)
         ((Tuple ok _) ok))))

  (define (bytes-concat (chunks : (List LispObject))) : LispObject
    (lisp LispObject (chunks) (%concat-bytes chunks)))

  (define (bytes-slice (bytes : LispObject) (start : U32) (end : U32)) : LispObject
    (lisp LispObject (bytes start end) (%slice-bytes bytes start end)))

  (define (u32-to-bytes (value : U32)) : LispObject
    (lisp LispObject (value) (%u32-to-bytes value)))

  (define (u32-from-bytes (bytes : LispObject) (start : U32)) : U32
    (lisp U32 (bytes start) (%u32-from-bytes bytes start)))

  (define (u8-to-u32 (value : U8)) : U32
    (lisp U32 (value) value))

  (define (bytes-zero32) : LispObject
    (lisp LispObject () (%zero-bytes 32)))

  (define (fixed-size (t : SSZType)) : (Tuple Boolean U32)
    (match t
      ((UintType n) (Tuple True (/ n 8)))
      ((BoolType) (Tuple True 1))
      ((ByteType) (Tuple True 1))
      ((BytesNType n) (Tuple True n))
      ((BitVectorType n)
       (let ((byte-len (lisp U32 (n) (ceiling n 8))))
         (Tuple True byte-len)))
      ((BitListType _)
       (Tuple False 0))
      ((ProgressiveListType _) (Tuple False 0))
      ((ProgressiveBitlistType) (Tuple False 0))
      ((NoneType) (Tuple True 0))
      ((UnionType _) (Tuple False 0))
      ((VectorType elem len)
       (match (fixed-size elem)
         ((Tuple True elem-size) (Tuple True (* elem-size len)))
         (_ (Tuple False 0))))
      ((ListType _ _) (Tuple False 0))
      ((ContainerType fields)
       (let ((acc (Tuple True 0)))
         (foldl
          (fn (state field)
            (match state
              ((Tuple True total)
               (match (fixed-size field)
                 ((Tuple True field-size) (Tuple True (+ total field-size)))
                 (_ (Tuple False 0))))
              (_ (Tuple False 0))))
          acc
          fields)))))

  (define (is-basic-type (t : SSZType)) : Boolean
    (match t
      ((UintType _) True)
      ((BoolType) True)
      ((ByteType) True)
      (_ False)))

  (define (size-of-basic (t : SSZType)) : U32
    (match t
      ((UintType n) (/ n 8))
      ((BoolType) 1)
      ((ByteType) 1)
      (_ (lisp U32 () (error "not a basic type")))))

  (define (chunk-count (t : SSZType)) : U32
    (match t
      ((UintType _) 1)
      ((BoolType) 1)
      ((ByteType) 1)
      ((BytesNType n)
       (lisp U32 (n) (%ceil-div n 32)))
      ((BitVectorType n)
       (lisp U32 (n) (%ceil-div (+ n 255) 256)))
      ((BitListType max-n)
       (lisp U32 (max-n) (%ceil-div (+ max-n 255) 256)))
      ((ProgressiveBitlistType) 1)
      ((VectorType elem len)
       (if (is-basic-type elem)
           (lisp U32 (len elem)
             (%ceil-div (* len (size-of-basic elem)) 32))
           len))
      ((ListType elem max-len)
       (if (is-basic-type elem)
           (lisp U32 (max-len elem)
             (%ceil-div (* max-len (size-of-basic elem)) 32))
           max-len))
      ((ProgressiveListType _) 1)
      ((ContainerType fields) (list-length fields))
      (_ 1)))

  (define (bytes-to-chunks (bytes : LispObject)) : (List LispObject)
    (lisp (List LispObject) (bytes) (%bytes-to-chunks bytes)))

  (define (merkleize (chunks : (List LispObject))) : LispObject
    (lisp LispObject (chunks) (%merkleize chunks nil)))

  (define (merkleize-limit (chunks : (List LispObject)) (limit : U32)) : LispObject
    (lisp LispObject (chunks limit) (%merkleize chunks limit)))

  (define (merkleize-progressive (chunks : (List LispObject))) : LispObject
    (lisp LispObject (chunks) (%merkleize-progressive chunks 1)))

  (define (mix-in-length (root : LispObject) (length : U32)) : LispObject
    (lisp LispObject (root length) (%mix-in-length root length)))

  (define (mix-in-selector (root : LispObject) (selector : U8)) : LispObject
    (lisp LispObject (root selector) (%mix-in-selector root selector)))

  (define (pack-bits (bits : (List Boolean))) : LispObject
    (let ((count (list-length bits)))
      (lisp LispObject (bits count) (%pack-bits bits count))))

  (define (encode-uint (n-bits : U32) (value : Integer)) : LispObject
    (lisp LispObject (n-bits value)
      (let* ((byte-len (/ n-bits 8))
             (max-value (ash 1 n-bits))
             (arr (make-array byte-len :element-type '(unsigned-byte 8))))
        (when (or (< value 0) (>= value max-value))
          (error "uint out of range for ~a bits: ~a" n-bits value))
        (dotimes (i byte-len arr)
          (setf (aref arr i) (ldb (byte 8 (* 8 i)) value))))))

  (define (encode-bool (value : Boolean)) : LispObject
    (lisp LispObject (value)
      (let ((arr (make-array 1 :element-type '(unsigned-byte 8))))
        (setf (aref arr 0) (if value 1 0))
        arr)))

  (define (encode-byte (value : U8)) : LispObject
    (lisp LispObject (value)
      (let ((arr (make-array 1 :element-type '(unsigned-byte 8))))
        (setf (aref arr 0) value)
        arr)))

  (define (encode-bytesn (n : U32) (bytes : LispObject)) : LispObject
    (lisp LispObject (n bytes)
      (let ((len (length bytes)))
        (unless (= len n)
          (error "bytesN length mismatch: expected ~a, got ~a" n len))
        bytes)))

  (define (encode-bitvector (n : U32) (bits : (List Boolean))) : LispObject
    (let ((count (list-length bits)))
      (if (/= count n)
          (lisp LispObject (count n) (error "bitvector length mismatch: expected ~a, got ~a" n count))
          (lisp LispObject (bits n) (%pack-bits bits n)))))

  (define (encode-bitlist (max-n : U32) (bits : (List Boolean))) : LispObject
    (let ((count (list-length bits)))
      (if (> count max-n)
          (lisp LispObject (count max-n) (error "bitlist length exceeds max: ~a > ~a" count max-n))
          (lisp LispObject (bits) (%pack-bitlist bits)))))

  (define (encode-progressive-list (elem : SSZType) (vals : (List SSZValue))) : LispObject
    (match (fixed-size elem)
      ((Tuple True _)
       (bytes-concat (map (fn (v) (encode v elem)) vals)))
      (_
       (let ((count (list-length vals))
             (encs (map (fn (v) (encode v elem)) vals)))
         (let ((fixed-size-bytes (* count 4)))
           (let ((offsets
                   (match (foldl
                           (fn (state enc)
                             (match state
                               ((Tuple current offs)
                                (Tuple (+ current (byte-array-length enc))
                                       (cons current offs)))))
                           (Tuple fixed-size-bytes Nil)
                           encs)
                     ((Tuple _ offs) offs))))
             (let ((offset-bytes (map (fn (o) (u32-to-bytes o)) (reverse offsets))))
               (bytes-concat (append offset-bytes encs))))))))

  (define (encode-progressive-bitlist (bits : (List Boolean))) : LispObject
    (lisp LispObject (bits) (%pack-bitlist bits)))

  (define (encode-union (types : (List SSZType)) (selector : U8) (val : SSZValue)) : LispObject
    (let* ((index (u8-to-u32 selector))
           (type-count (list-length types)))
      (if (>= index type-count)
          (lisp LispObject (index type-count) (error "union selector out of bounds"))
          (let ((selected (list-nth types index)))
            (match selected
              ((NoneType)
               (match val
                 ((VUnionNone sel)
                  (if (/= sel selector)
                      (lisp LispObject (sel selector) (error "union selector mismatch"))
                      (if (/= selector 0)
                          (lisp LispObject (selector) (error "union none requires selector 0"))
                          (encode-byte selector))))
                 (_ (lisp LispObject () (error "union none requires VUnionNone")))))
              (_
               (match val
                 ((VUnionSome sel inner)
                  (if (/= sel selector)
                      (lisp LispObject (sel selector) (error "union selector mismatch"))
                      (bytes-concat (list (encode-byte selector) (encode inner selected)))))
                 (_ (lisp LispObject () (error "union value requires VUnionSome"))))))))))

  (define (encode-vector (elem : SSZType) (len : U32) (vals : (List SSZValue))) : LispObject
    (let ((count (list-length vals)))
      (if (/= count len)
          (lisp LispObject (count len) (error "vector length mismatch: expected ~a, got ~a" len count))
          (match (fixed-size elem)
            ((Tuple True _)
             (bytes-concat (map (fn (v) (encode v elem)) vals)))
            (_
             (let ((fixed-size-bytes (* len 4))
                   (encs (map (fn (v) (encode v elem)) vals)))
               (let ((offsets
                       (match (foldl
                               (fn (state enc)
                                 (match state
                                   ((Tuple current offs)
                                    (Tuple (+ current (byte-array-length enc))
                                           (cons current offs)))))
                               (Tuple fixed-size-bytes Nil)
                               encs)
                         ((Tuple _ offs) offs))))
                 (let ((offset-bytes (map (fn (o) (u32-to-bytes o)) (reverse offsets))))
                   (bytes-concat (append offset-bytes encs)))))))))

  (define (encode-list (elem : SSZType) (max-len : U32) (vals : (List SSZValue))) : LispObject
    (let ((count (list-length vals)))
      (if (> count max-len)
          (lisp LispObject (count max-len) (error "list length exceeds max: ~a > ~a" count max-len))
          (match (fixed-size elem)
            ((Tuple True _)
             (bytes-concat (map (fn (v) (encode v elem)) vals)))
            (_
             (let ((fixed-size-bytes (* count 4))
                   (encs (map (fn (v) (encode v elem)) vals)))
               (let ((offsets
                       (match (foldl
                               (fn (state enc)
                                 (match state
                                   ((Tuple current offs)
                                    (Tuple (+ current (byte-array-length enc))
                                           (cons current offs)))))
                               (Tuple fixed-size-bytes Nil)
                               encs)
                         ((Tuple _ offs) offs))))
                 (let ((offset-bytes (map (fn (o) (u32-to-bytes o)) (reverse offsets))))
                   (bytes-concat (append offset-bytes encs)))))))))

  (define (encode-container (fields : (List SSZType)) (vals : (List SSZValue))) : LispObject
    (let ((field-count (list-length fields))
          (val-count (list-length vals)))
      (if (/= field-count val-count)
          (lisp LispObject (field-count val-count) (error "container field/value mismatch: ~a vs ~a" field-count val-count))
          (match (fixed-size (ContainerType fields))
            ((Tuple True _)
             (bytes-concat
              (map (fn (pair)
                     (match pair
                       ((Tuple val field) (encode val field))))
                   (zip vals fields))))
            (_
             (let ((fixed-size-bytes
                     (foldl
                      (fn (acc field)
                        (match (fixed-size field)
                          ((Tuple True field-size) (+ acc field-size))
                          (_ (+ acc 4))))
                      0
                      fields)))
               (let ((state
                       (foldl
                        (fn (st pair)
                          (match st
                            ((Tuple current fixed-chunks var-chunks)
                             (match pair
                               ((Tuple val field)
                                (match (fixed-size field)
                                  ((Tuple True _)
                                   (Tuple current
                                          (cons (encode val field) fixed-chunks)
                                          var-chunks))
                                  (_
                                   (let ((enc (encode val field)))
                                     (Tuple (+ current (byte-array-length enc))
                                            (cons (u32-to-bytes current) fixed-chunks)
                                            (cons enc var-chunks)))))))))
                        (Tuple fixed-size-bytes Nil Nil)
                        (zip vals fields))))
                 (match state
                   ((Tuple _ fixed-chunks var-chunks)
                    (bytes-concat (append (reverse fixed-chunks) (reverse var-chunks)))))))))))

  (define (decode-uint (n-bits : U32) (bytes : LispObject)) : Integer
    (lisp Integer (n-bits bytes)
      (let* ((byte-len (/ n-bits 8))
             (len (length bytes)))
        (unless (= len byte-len)
          (error "uint byte length mismatch: expected ~a, got ~a" byte-len len))
        (let ((acc 0))
          (dotimes (i byte-len acc)
            (setf acc (+ acc (ash (aref bytes i) (* 8 i)))))))))

  (define (decode-bool (bytes : LispObject)) : Boolean
    (lisp Boolean (bytes)
      (let ((len (length bytes)))
        (unless (= len 1)
          (error "bool byte length mismatch: expected 1, got ~a" len))
        (let ((b (aref bytes 0)))
          (cond ((= b 0) nil)
                ((= b 1) t)
                (t (error "invalid bool value: ~a" b)))))))

  (define (decode-byte (bytes : LispObject)) : U8
    (lisp U8 (bytes)
      (let ((len (length bytes)))
        (unless (= len 1)
          (error "byte length mismatch: expected 1, got ~a" len))
        (aref bytes 0))))

  (define (decode-bytesn (n : U32) (bytes : LispObject)) : LispObject
    (lisp LispObject (n bytes)
      (let ((len (length bytes)))
        (unless (= len n)
          (error "bytesN length mismatch: expected ~a, got ~a" n len))
        bytes)))

  (define (decode-bitvector (n : U32) (bytes : LispObject)) : (List Boolean)
    (lisp (List Boolean) (bytes n)
      (progn
        (%validate-bitvector-tail bytes n)
        (%unpack-bitvector bytes n))))

  (define (decode-bitlist (max-n : U32) (bytes : LispObject)) : (List Boolean)
    (lisp (List Boolean) (bytes max-n)
      (%unpack-bitlist bytes max-n)))

  (define (decode-progressive-list (elem : SSZType) (bytes : LispObject)) : (List SSZValue)
    (match (fixed-size elem)
      ((Tuple True elem-size)
       (let ((total (byte-array-length bytes)))
         (if (= elem-size 0)
             Nil
             (if (/= (mod total elem-size) 0)
                 (lisp (List SSZValue) (total elem-size) (error "progressive list byte length mismatch"))
                 (let ((count (/ total elem-size)))
                   (map (fn (i)
                          (decode (bytes-slice bytes (* i elem-size) (* (+ i 1) elem-size)) elem))
                        (range-u32 0 count)))))))
      (_
       (let ((total (byte-array-length bytes)))
         (if (= total 0)
             Nil
             (let ((first-offset (u32-from-bytes bytes 0)))
               (if (/= (mod first-offset 4) 0)
                   (lisp (List SSZValue) (first-offset) (error "invalid progressive list offset"))
                   (let ((count (/ first-offset 4)))
                     (let ((offsets
                             (map (fn (i) (u32-from-bytes bytes (* i 4)))
                                  (range-u32 0 count))))
                       (if (not (validate-offsets offsets total first-offset))
                           (lisp (List SSZValue) (offsets) (error "invalid progressive list offsets"))
                           (map (fn (i)
                                  (let ((start (list-nth offsets i))
                                        (end (if (= (+ i 1) count)
                                                 total
                                                 (list-nth offsets (+ i 1)))))
                                    (decode (bytes-slice bytes start end) elem)))
                                (range-u32 0 count)))))))))))

  (define (decode-progressive-bitlist (bytes : LispObject)) : (List Boolean)
    (let ((max-bits (* (byte-array-length bytes) 8)))
      (lisp (List Boolean) (bytes max-bits)
        (%unpack-bitlist bytes max-bits))))

  (define (decode-union (types : (List SSZType)) (bytes : LispObject)) : SSZValue
    (let ((total (byte-array-length bytes)))
      (if (< total 1)
          (lisp SSZValue () (error "union requires selector byte"))
          (let* ((selector (byte-array-ref bytes 0))
                 (index (u8-to-u32 selector))
                 (type-count (list-length types)))
            (if (>= index type-count)
                (lisp SSZValue (index type-count) (error "union selector out of bounds"))
                (let ((selected (list-nth types index)))
                  (match selected
                    ((NoneType)
                     (if (/= selector 0)
                         (lisp SSZValue (selector) (error "union none requires selector 0"))
                         (if (/= total 1)
                          (lisp SSZValue (total) (error "union none must be selector only"))
                          (VUnionNone selector))))
                    (_
                     (let ((payload (bytes-slice bytes 1 total)))
                       (VUnionSome selector (decode payload selected)))))))))))

  (define (decode-vector (elem : SSZType) (len : U32) (bytes : LispObject)) : (List SSZValue)
    (match (fixed-size elem)
      ((Tuple True elem-size)
       (let ((total (byte-array-length bytes)))
         (if (/= total (* len elem-size))
             (lisp (List SSZValue) (total len elem-size) (error "vector byte length mismatch"))
             (map (fn (i)
                    (decode (bytes-slice bytes (* i elem-size) (* (+ i 1) elem-size)) elem))
                  (range-u32 0 len)))))
      (_
       (let ((total (byte-array-length bytes)))
         (if (= total 0)
             (if (= len 0)
                 Nil
                 (lisp (List SSZValue) (len) (error "empty vector encoding")))
             (let ((first-offset (u32-from-bytes bytes 0)))
               (if (/= (mod first-offset 4) 0)
                   (lisp (List SSZValue) (first-offset) (error "invalid vector offset"))
                   (let ((count (/ first-offset 4)))
                     (if (/= count len)
                         (lisp (List SSZValue) (count len) (error "vector offset count mismatch"))
                         (let ((offsets
                                 (map (fn (i) (u32-from-bytes bytes (* i 4)))
                                      (range-u32 0 len))))
                           (if (not (validate-offsets offsets total (* len 4)))
                               (lisp (List SSZValue) (offsets) (error "invalid vector offsets"))
                               (map (fn (i)
                                      (let ((start (list-nth offsets i))
                                            (end (if (= (+ i 1) len)
                                                     total
                                                     (list-nth offsets (+ i 1)))))
                                        (decode (bytes-slice bytes start end) elem)))
                                    (range-u32 0 len))))))))))))))

  (define (decode-list (elem : SSZType) (max-len : U32) (bytes : LispObject)) : (List SSZValue)
    (match (fixed-size elem)
      ((Tuple True elem-size)
       (let ((total (byte-array-length bytes)))
         (if (/= (mod total elem-size) 0)
             (lisp (List SSZValue) (total elem-size) (error "list byte length mismatch"))
             (let ((count (/ total elem-size)))
               (if (> count max-len)
                   (lisp (List SSZValue) (count max-len) (error "list length exceeds max"))
                   (map (fn (i)
                          (decode (bytes-slice bytes (* i elem-size) (* (+ i 1) elem-size)) elem))
                        (range-u32 0 count)))))))
      (_
       (let ((total (byte-array-length bytes)))
         (if (= total 0)
             Nil
             (let ((first-offset (u32-from-bytes bytes 0)))
               (if (/= (mod first-offset 4) 0)
                   (lisp (List SSZValue) (first-offset) (error "invalid list offset"))
                   (let ((count (/ first-offset 4)))
                     (if (> count max-len)
                         (lisp (List SSZValue) (count max-len) (error "list length exceeds max"))
                         (let ((offsets
                                 (map (fn (i) (u32-from-bytes bytes (* i 4)))
                                      (range-u32 0 count))))
                           (if (not (validate-offsets offsets total first-offset))
                               (lisp (List SSZValue) (offsets) (error "invalid list offsets"))
                               (map (fn (i)
                                      (let ((start (list-nth offsets i))
                                            (end (if (= (+ i 1) count)
                                                     total
                                                     (list-nth offsets (+ i 1)))))
                                        (decode (bytes-slice bytes start end) elem)))
                                    (range-u32 0 count))))))))))))

  (define (container-offsets (fields : (List SSZType)) (bytes : LispObject)) : (List U32)
    (let ((state
            (foldl
             (fn (st field)
               (match st
                 ((Tuple pos offs)
                  (match (fixed-size field)
                    ((Tuple True size) (Tuple (+ pos size) offs))
                    (_ (Tuple (+ pos 4) (cons (u32-from-bytes bytes pos) offs)))))))
             (Tuple 0 Nil)
             fields)))
      (match state
        ((Tuple _ offs) (reverse offs)))))

  (define (container-fixed-size (fields : (List SSZType))) : U32
    (foldl
     (fn (acc field)
       (match (fixed-size field)
         ((Tuple True size) (+ acc size))
         (_ (+ acc 4))))
     0
     fields))

  (define (decode-container (fields : (List SSZType)) (bytes : LispObject)) : (List SSZValue)
    (match (fixed-size (ContainerType fields))
      ((Tuple True _)
       (let ((state
               (foldl
                (fn (st field)
                  (match st
                    ((Tuple pos acc)
                     (match (fixed-size field)
                       ((Tuple True size)
                        (Tuple (+ pos size)
                               (cons (decode (bytes-slice bytes pos (+ pos size)) field) acc)))
                       (_ (lisp (Tuple U32 (List SSZValue)) (field) (error "unexpected variable field")))))))
                (Tuple 0 Nil)
                fields)))
         (match state
           ((Tuple _ acc) (reverse acc)))))
      (_
       (let ((total (byte-array-length bytes))
             (fixed-size-bytes (container-fixed-size fields))
             (offsets (container-offsets fields bytes)))
         (if (not (validate-offsets offsets total fixed-size-bytes))
             (lisp (List SSZValue) (offsets) (error "invalid container offsets"))
             (let ((state
                 (foldl
                  (fn (st field)
                    (match st
                      ((Tuple pos idx acc)
                       (match (fixed-size field)
                         ((Tuple True size)
                          (Tuple (+ pos size)
                                 idx
                                 (cons (decode (bytes-slice bytes pos (+ pos size)) field) acc)))
                         (_
                          (let ((start (list-nth offsets idx))
                                (end (if (= (+ idx 1) (list-length offsets))
                                         total
                                         (list-nth offsets (+ idx 1)))))
                            (Tuple (+ pos 4)
                                   (+ idx 1)
                                   (cons (decode (bytes-slice bytes start end) field) acc))))))))
                  (Tuple 0 0 Nil)
                  fields)))
              (match state
                ((Tuple _ _ acc) (reverse acc)))))))))

  (define (hash-tree-root (value : SSZValue) (t : SSZType)) : LispObject
    (match (Tuple value t)
      ((Tuple (VUInt _) (UintType _))
       (merkleize (bytes-to-chunks (encode value t))))
      ((Tuple (VBool _) (BoolType))
       (merkleize (bytes-to-chunks (encode value t))))
      ((Tuple (VByte _) (ByteType))
       (merkleize (bytes-to-chunks (encode value t))))
      ((Tuple (VBytes _) (BytesNType _))
       (merkleize (bytes-to-chunks (encode value t))))
      ((Tuple (VBitVector bits) (BitVectorType n))
       (if (/= (list-length bits) n)
           (lisp LispObject (n) (error "bitvector length mismatch for hash_tree_root"))
           (merkleize-limit (bytes-to-chunks (pack-bits bits)) (chunk-count t))))
      ((Tuple (VBitList bits) (BitListType max-n))
       (if (> (list-length bits) max-n)
           (lisp LispObject (max-n) (error "bitlist length exceeds max for hash_tree_root"))
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
      ((Tuple (VVector vals) (VectorType elem _))
       (if (is-basic-type elem)
           (merkleize (bytes-to-chunks (encode value t)))
           (let ((roots (map (fn (v) (hash-tree-root v elem)) vals)))
             (merkleize roots))))
      ((Tuple (VList vals) (ListType elem _))
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
      ((Tuple (VUnionNone selector) (UnionType types))
       (if (/= selector 0)
           (lisp LispObject (selector) (error "union none requires selector 0"))
           (mix-in-selector (bytes-zero32) selector)))
      ((Tuple (VUnionSome selector val) (UnionType types))
       (let* ((index (u8-to-u32 selector))
              (type-count (list-length types)))
         (if (>= index type-count)
             (lisp LispObject (index type-count) (error "union selector out of bounds"))
             (let ((selected (list-nth types index)))
               (match selected
                 ((NoneType) (lisp LispObject () (error "union none selected with value")))
                 (_ (mix-in-selector (hash-tree-root val selected) selector)))))))
      (_ (lisp LispObject () (error "type/value mismatch")))))

  (define (encode (value : SSZValue) (t : SSZType)) : LispObject
    (match (Tuple value t)
      ((Tuple (VUInt v) (UintType n)) (encode-uint n v))
      ((Tuple (VBool v) (BoolType)) (encode-bool v))
      ((Tuple (VByte v) (ByteType)) (encode-byte v))
      ((Tuple (VBytes v) (BytesNType n)) (encode-bytesn n v))
      ((Tuple (VBitVector v) (BitVectorType n)) (encode-bitvector n v))
      ((Tuple (VBitList v) (BitListType max-n)) (encode-bitlist max-n v))
      ((Tuple (VList v) (ProgressiveListType elem)) (encode-progressive-list elem v))
      ((Tuple (VBitList v) (ProgressiveBitlistType)) (encode-progressive-bitlist v))
      ((Tuple (VUnionNone sel) (UnionType types)) (encode-union types sel value))
      ((Tuple (VUnionSome sel _) (UnionType types)) (encode-union types sel value))
      ((Tuple (VVector v) (VectorType elem len)) (encode-vector elem len v))
      ((Tuple (VList v) (ListType elem max-len)) (encode-list elem max-len v))
      ((Tuple (VContainer v) (ContainerType fields)) (encode-container fields v))
      (_ (lisp LispObject () (error "type/value mismatch")))))

  (define (decode (bytes : LispObject) (t : SSZType)) : SSZValue
    (match t
      ((UintType n) (VUInt (decode-uint n bytes)))
      ((BoolType) (VBool (decode-bool bytes)))
      ((ByteType) (VByte (decode-byte bytes)))
      ((BytesNType n) (VBytes (decode-bytesn n bytes)))
      ((BitVectorType n) (VBitVector (decode-bitvector n bytes)))
      ((BitListType max-n) (VBitList (decode-bitlist max-n bytes)))
      ((ProgressiveListType elem) (VList (decode-progressive-list elem bytes)))
      ((ProgressiveBitlistType) (VBitList (decode-progressive-bitlist bytes)))
      ((UnionType types) (decode-union types bytes))
      ((VectorType elem len) (VVector (decode-vector elem len bytes)))
      ((ListType elem max-len) (VList (decode-list elem max-len bytes)))
      ((ContainerType fields) (VContainer (decode-container fields bytes))))))
