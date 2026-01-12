(in-package #:ssz-test)

(def-suite ssz-basic)
(in-suite ssz-basic)

(defun make-bytes (&rest octets)
  (let* ((len (length octets))
         (arr (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len arr)
      (setf (aref arr i) (nth i octets)))))

(test encode-uint16
  (let* ((typ (make-uint-type 16))
         (val (vuint #x1234))
         (bytes (encode val typ)))
    (is (= 2 (byte-array-length bytes)))
    (is (= #x34 (byte-array-ref bytes 0)))
    (is (= #x12 (byte-array-ref bytes 1)))))

(test decode-uint16
  (let* ((typ (make-uint-type 16))
         (bytes (make-bytes #x34 #x12))
         (val (decode bytes typ)))
    (is (typep val 'ssz::ssz-value))
    (is (equal (ssz::vuint #x1234) val))))

(test encode-bool
  (let* ((typ (make-bool-type))
         (bytes (encode (vbool t) typ)))
    (is (= 1 (byte-array-length bytes)))
    (is (= 1 (byte-array-ref bytes 0)))))

(test decode-bool
  (let* ((typ (make-bool-type))
         (bytes (make-bytes 0))
         (val (decode bytes typ)))
    (is (equal (vbool nil) val))))

(test encode-bytesn
  (let* ((typ (make-bytesn-type 4))
         (bytes (make-bytes 1 2 3 4))
         (out (encode (vbytes bytes) typ)))
    (is (= 4 (byte-array-length out)))
    (is (= 3 (byte-array-ref out 2)))))

(test encode-vector-fixed
  (let* ((typ (make-vector-type (make-uint-type 16) 3))
         (val (vvector (list (vuint 1) (vuint 2) (vuint 3))))
         (bytes (encode val typ)))
    (is (= 6 (byte-array-length bytes)))
    (is (= 1 (byte-array-ref bytes 0)))
    (is (= 0 (byte-array-ref bytes 1)))
    (is (= 3 (byte-array-ref bytes 4)))))

(test encode-list-fixed
  (let* ((typ (make-list-type (make-uint-type 16) 4))
         (val (vlist (list (vuint 10) (vuint 20))))
         (bytes (encode val typ)))
    (is (= 4 (byte-array-length bytes)))
    (is (= #x0a (byte-array-ref bytes 0)))
    (is (= #x14 (byte-array-ref bytes 2)))))

(test encode-vector-variable
  (let* ((elem (make-list-type (make-uint-type 16) 4))
         (typ (make-vector-type elem 2))
         (val (vvector (list (vlist (list (vuint 1) (vuint 2)))
                             (vlist (list (vuint 3))))))
         (bytes (encode val typ)))
    (is (= 14 (byte-array-length bytes)))
    (is (= 8 (byte-array-ref bytes 0)))
    (is (= 0 (byte-array-ref bytes 1)))
    (is (= 12 (byte-array-ref bytes 4)))
    (is (= 0 (byte-array-ref bytes 5)))
    (is (= 1 (byte-array-ref bytes 8)))
    (is (= 2 (byte-array-ref bytes 10)))
    (is (= 3 (byte-array-ref bytes 12)))))

(test encode-container-variable
  (let* ((fields (list (make-uint-type 16)
                       (make-list-type (make-uint-type 16) 4)))
         (typ (make-container-type fields))
         (val (vcontainer (list (vuint #x0102)
                                (vlist (list (vuint 3) (vuint 4))))))
         (bytes (encode val typ)))
    (is (= 10 (byte-array-length bytes)))
    (is (= #x02 (byte-array-ref bytes 0)))
    (is (= #x01 (byte-array-ref bytes 1)))
    (is (= 6 (byte-array-ref bytes 2)))
    (is (= 0 (byte-array-ref bytes 3)))
    (is (= #x03 (byte-array-ref bytes 6)))
    (is (= #x04 (byte-array-ref bytes 8)))))

(test decode-container-variable
  (let* ((fields (list (make-uint-type 16)
                       (make-list-type (make-uint-type 16) 4)))
         (typ (make-container-type fields))
         (bytes (make-bytes #x02 #x01 6 0 0 0 #x03 0 #x04 0))
         (val (decode bytes typ)))
    (is (equal (vcontainer (list (vuint #x0102)
                                 (vlist (list (vuint 3) (vuint 4)))))
               val))))

(test encode-bitvector
  (let* ((typ (make-bitvector-type 10))
         (val (vbitvector (list t nil t nil nil nil t nil nil t)))
         (bytes (encode val typ)))
    (is (= 2 (byte-array-length bytes)))
    (is (= #b01000101 (byte-array-ref bytes 0)))
    (is (= #b00000010 (byte-array-ref bytes 1)))))

(test decode-bitvector
  (let* ((typ (make-bitvector-type 5))
         (bytes (make-bytes #b00010101))
         (val (decode bytes typ)))
    (is (equal (vbitvector (list t nil t nil t)) val))))

(test encode-bitlist
  (let* ((typ (make-bitlist-type 6))
         (val (vbitlist (list t nil t)))
         (bytes (encode val typ)))
    (is (= 1 (byte-array-length bytes)))
    (is (= #b00001101 (byte-array-ref bytes 0)))))

(test decode-bitlist
  (let* ((typ (make-bitlist-type 6))
         (bytes (make-bytes #b00001101))
         (val (decode bytes typ)))
    (is (equal (vbitlist (list t nil t)) val))))

(test encode-union-none
  (let* ((typ (make-union-type (list (make-none-type) (make-uint-type 16))))
         (val (vunion-none 0))
         (bytes (encode val typ)))
    (is (= 1 (byte-array-length bytes)))
    (is (= 0 (byte-array-ref bytes 0)))))

(test decode-union-none
  (let* ((typ (make-union-type (list (make-none-type) (make-uint-type 16))))
         (bytes (make-bytes 0))
         (val (decode bytes typ)))
    (is (equal (vunion-none 0) val))))

(test encode-union-value
  (let* ((typ (make-union-type (list (make-none-type) (make-uint-type 16))))
         (val (vunion 1 (vuint #x1234)))
         (bytes (encode val typ)))
    (is (= 3 (byte-array-length bytes)))
    (is (= 1 (byte-array-ref bytes 0)))
    (is (= #x34 (byte-array-ref bytes 1)))
    (is (= #x12 (byte-array-ref bytes 2)))))

(test decode-union-value
  (let* ((typ (make-union-type (list (make-none-type) (make-uint-type 16))))
         (bytes (make-bytes 1 #x34 #x12))
         (val (decode bytes typ)))
    (is (equal (vunion 1 (vuint #x1234)) val))))

(test encode-progressive-list
  (let* ((typ (make-progressive-list-type (make-uint-type 16)))
         (val (vlist (list (vuint 1) (vuint 2))))
         (bytes (encode val typ)))
    (is (= 4 (byte-array-length bytes)))
    (is (= 1 (byte-array-ref bytes 0)))
    (is (= 2 (byte-array-ref bytes 2)))))

(test decode-progressive-bitlist
  (let* ((typ (make-progressive-bitlist-type))
         (bytes (make-bytes #b00001101))
         (val (decode bytes typ)))
    (is (equal (vbitlist (list t nil t)) val))))
