(in-package #:ssz-test)

(def-suite ssz-generic)
(in-suite ssz-generic)

(defparameter *ssz-generic-root*
  (uiop:getenv "SSZ_GENERIC_DIR"))

(defparameter *ssz-generic-schema* nil)
(defparameter *ssz-generic-type-cache* (make-hash-table :test 'equal))

(defun %string-prefix-p (prefix s)
  (and (<= (length prefix) (length s))
       (string= prefix (subseq s 0 (length prefix)))))

(defun %parse-decimal-from (s start)
  (let ((end start))
    (loop while (and (< end (length s))
                     (digit-char-p (char s end)))
          do (incf end))
    (values (parse-integer s :start start :end end) end)))

(defun %parse-uint-bits (s prefix)
  (when (%string-prefix-p prefix s)
    (%parse-decimal-from s (length prefix))))

(defun %hex-digit (ch)
  (or (digit-char-p ch)
      (let ((uc (char-upcase ch)))
        (cond ((and (char>= uc #\A) (char<= uc #\F))
               (+ 10 (- (char-code uc) (char-code #\A))))
              (t nil)))))

(defun %parse-hex-bytes (s)
  (let* ((str (string-trim "'\"" s))
         (hex (if (%string-prefix-p "0x" str)
                  (subseq str 2)
                  str)))
    (when (oddp (length hex))
      (setf hex (concatenate 'string "0" hex)))
    (let* ((len (/ (length hex) 2))
           (out (make-array len :element-type '(unsigned-byte 8))))
      (dotimes (i len out)
        (let* ((hi (%hex-digit (char hex (* 2 i))))
               (lo (%hex-digit (char hex (+ (* 2 i) 1)))))
          (unless (and hi lo)
            (error "invalid hex string: ~a" s))
          (setf (aref out i) (+ (* 16 hi) lo)))))))

(defun %parse-integer-value (v)
  (cond ((integerp v) v)
        ((stringp v)
         (let ((str (string-trim "'\"" v)))
           (if (%string-prefix-p "0x" str)
               (parse-integer str :start 2 :radix 16)
               (parse-integer str))))
        (t (error "unsupported integer value: ~a" v))))

(defun %yaml-parse-file (path)
  (let* ((pkg (or (find-package :yaml) (find-package :cl-yaml)))
         (parse-file (and pkg (or (find-symbol "PARSE-FILE" pkg)
                                  (find-symbol "PARSE" pkg)))))
    (unless (and parse-file (fboundp parse-file))
      (error "cl-yaml not available; install cl-yaml to run ssz_generic tests"))
    (if (string= (symbol-name parse-file) "PARSE-FILE")
        (funcall parse-file path)
        (with-open-file (in path)
          (let ((content (make-string (file-length in))))
            (read-sequence content in)
            (funcall parse-file content))))))

(defun %load-schema ()
  (when (null *ssz-generic-schema*)
    (let ((path (uiop:getenv "SSZ_GENERIC_SCHEMA")))
      (when (and path (not (string= path "")) (probe-file path))
        (setf *ssz-generic-schema* (%yaml-parse-file path))))))

(defun %yaml->list (v)
  (cond ((listp v) v)
        ((vectorp v) (coerce v 'list))
        (t (error "expected list value, got: ~a" v))))

(defun %normalize-key (k)
  (cond ((stringp k) (string-downcase k))
        ((symbolp k) (string-downcase (symbol-name k)))
        (t (string-downcase (princ-to-string k)))))

(defun %mapping->hash (mapping)
  (let ((ht (make-hash-table :test 'equal)))
    (cond
      ((hash-table-p mapping)
       (maphash (lambda (k v)
                  (setf (gethash (%normalize-key k) ht) v))
                mapping))
      ((and (listp mapping)
            (evenp (length mapping))
            (keywordp (first mapping)))
       (loop for (k v) on mapping by #'cddr do
         (setf (gethash (%normalize-key k) ht) v)))
      ((listp mapping)
       (dolist (pair mapping)
         (when (consp pair)
           (setf (gethash (%normalize-key (car pair)) ht) (cdr pair))))))
    ht))

(defun %parse-top-level (s ch)
  (let ((depth 0)
        (parts nil)
        (start 0))
    (dotimes (i (length s))
      (let ((c (char s i)))
        (cond
          ((char= c #\[) (incf depth))
          ((char= c #\]) (decf depth))
          ((and (char= c ch) (= depth 0))
           (push (subseq s start i) parts)
           (setf start (1+ i))))))    
    (push (subseq s start) parts)
    (nreverse parts)))

(defun %schema-containers ()
  (let* ((schema (%mapping->hash *ssz-generic-schema*))
         (containers (gethash "containers" schema)))
    (and containers (%mapping->hash containers))))

(defun %schema-progressive-containers ()
  (let* ((schema (%mapping->hash *ssz-generic-schema*))
         (containers (gethash "progressive_containers" schema)))
    (and containers (%mapping->hash containers))))

(defun %resolve-container-desc (name containers &optional progressive-containers)
  (let ((key (%normalize-key name)))
    (or (gethash key *ssz-generic-type-cache*)
        (let ((entry (gethash key containers)))
          (when entry
            (setf (gethash key *ssz-generic-type-cache*) :in-progress)
            (let* ((fields-raw (%yaml->list entry))
                   (fields
                    (mapcar
                     (lambda (field)
                       (let* ((fmap (%mapping->hash field))
                              (fname (or (gethash "name" fmap) (car field)))
                              (type-str (or (gethash "type" fmap) (cdr field)))
                              (desc (%parse-type-desc type-str containers progressive-containers)))
                         (cons fname desc)))
                     fields-raw))
                   (types (mapcar (lambda (f) (getf (cdr f) :ssz-type)) fields))
                   (desc (list :kind :container
                               :fields fields
                               :ssz-type (ssz:make-container-type types))))
              (setf (gethash key *ssz-generic-type-cache*) desc)
              desc))))))

(defun %resolve-progressive-container-desc (name containers progressive-containers)
  (let ((key (%normalize-key name)))
    (or (gethash key *ssz-generic-type-cache*)
        (let ((entry (gethash key progressive-containers)))
          (when entry
            (setf (gethash key *ssz-generic-type-cache*) :in-progress)
            (let* ((entry-map (%mapping->hash entry))
                   (active-raw (or (gethash "active_fields" entry-map)
                                   (gethash "active-fields" entry-map)))
                   (fields-raw (or (gethash "fields" entry-map) entry))
                   (active-fields
                    (mapcar (lambda (v) (not (zerop (%parse-integer-value v))))
                            (%yaml->list active-raw)))
                   (fields
                    (mapcar
                     (lambda (field)
                       (let* ((fmap (%mapping->hash field))
                              (fname (or (gethash "name" fmap) (car field)))
                              (type-str (or (gethash "type" fmap) (cdr field)))
                              (desc (%parse-type-desc type-str containers progressive-containers)))
                         (cons fname desc)))
                     (%yaml->list fields-raw)))
                   (types (mapcar (lambda (f) (getf (cdr f) :ssz-type)) fields))
                   (desc (list :kind :progcontainer
                               :fields fields
                               :active-fields active-fields
                               :ssz-type (ssz:make-progressive-container-type types active-fields))))
              (setf (gethash key *ssz-generic-type-cache*) desc)
              desc))))))

(defun %parse-type-desc (s &optional containers progressive-containers)
  (let* ((raw (string-trim " " (string-downcase s))))
    (cond
      ((string= raw "bool")
       (list :kind :bool :ssz-type (ssz:make-bool-type)))
      ((string= raw "byte")
       (list :kind :byte :ssz-type (ssz:make-byte-type)))
      ((%string-prefix-p "uint" raw)
       (multiple-value-bind (bits _) (%parse-uint-bits raw "uint")
         (declare (ignore _))
         (list :kind :uint :bits bits :ssz-type (ssz:make-uint-type bits))))
      ((%string-prefix-p "bytes" raw)
       (multiple-value-bind (n _) (%parse-uint-bits raw "bytes")
         (declare (ignore _))
         (list :kind :bytesn :len n :ssz-type (ssz:make-bytesn-type n))))
      ((%string-prefix-p "bitvector[" raw)
       (let* ((inner (subseq raw (length "bitvector[") (1- (length raw))))
              (n (parse-integer (string-trim " " inner))))
         (list :kind :bitvector :bits n :ssz-type (ssz:make-bitvector-type n))))
      ((%string-prefix-p "bitlist[" raw)
       (let* ((inner (subseq raw (length "bitlist[") (1- (length raw))))
              (n (parse-integer (string-trim " " inner))))
         (list :kind :bitlist :max-bits n :ssz-type (ssz:make-bitlist-type n))))
      ((or (%string-prefix-p "list[" raw) (%string-prefix-p "vector[" raw))
       (let* ((is-list (%string-prefix-p "list[" raw))
              (inner (subseq raw (if is-list (length "list[") (length "vector["))
                            (1- (length raw))))
              (parts (%parse-top-level inner #\,)))
         (unless (= (length parts) 2)
           (error "invalid list/vector type: ~a" s))
         (let* ((elem-desc (%parse-type-desc (string-trim " " (first parts))
                                             containers
                                             progressive-containers))
                (len (parse-integer (string-trim " " (second parts)))))
           (if is-list
               (list :kind :list :elem elem-desc :max-len len
                     :ssz-type (ssz:make-list-type (getf elem-desc :ssz-type) len))
               (list :kind :vector :elem elem-desc :len len
                     :ssz-type (ssz:make-vector-type (getf elem-desc :ssz-type) len))))))
      ((or (string= raw "progressivebitlist")
           (string= raw "progressive_bitlist"))
       (list :kind :progbitlist :ssz-type (ssz:make-progressive-bitlist-type)))
      ((or (%string-prefix-p "progressivelist[" raw)
           (%string-prefix-p "progressive_list[" raw))
       (let* ((inner (subseq raw (if (%string-prefix-p "progressivelist[" raw)
                                     (length "progressivelist[")
                                     (length "progressive_list["))
                             (1- (length raw))))
              (elem-desc (%parse-type-desc (string-trim " " inner) containers progressive-containers)))
         (list :kind :proglist :elem elem-desc
               :ssz-type (ssz:make-progressive-list-type (getf elem-desc :ssz-type)))))
      (t
       (when containers
         (let ((resolved (%resolve-container-desc raw containers progressive-containers)))
           (when resolved
             (return-from %parse-type-desc resolved))))
       (when progressive-containers
         (let ((resolved (%resolve-progressive-container-desc raw containers progressive-containers)))
           (when resolved
             (return-from %parse-type-desc resolved))))
       (error "unknown type: ~a" s)))))

(defun %yaml->bool (v)
  (cond ((eq v t) t)
        ((eq v nil) nil)
        ((stringp v)
         (let ((s (string-downcase (string-trim "'\"" v))))
           (cond ((string= s "true") t)
                 ((string= s "false") nil)
                 (t (error "invalid boolean: ~a" v)))))
        (t (error "invalid boolean: ~a" v))))

(defun %unpack-bitvector (bytes bit-count)
  (ssz::%unpack-bitvector bytes bit-count))

(defun %unpack-bitlist (bytes max-bits)
  (ssz::%unpack-bitlist bytes max-bits))

(defun %make-uint-type (bits)
  (ssz:make-uint-type bits))

(defun %make-basic-type (name)
  (cond ((string= name "bool") (ssz:make-bool-type))
        ((%string-prefix-p "uint" name)
         (multiple-value-bind (bits _) (%parse-uint-bits name "uint")
           (declare (ignore _))
           (ssz:make-uint-type bits)))
        (t (error "unknown basic type: ~a" name))))

(defun %yaml->ssz (desc yaml-value)
  (case (getf desc :kind)
    (:bool (ssz:make-vbool (%yaml->bool yaml-value)))
    (:uint (ssz:make-vuint (%parse-integer-value yaml-value)))
    (:byte (ssz:make-vbyte (%parse-integer-value yaml-value)))
    (:bytesn (ssz:make-vbytes (%parse-hex-bytes yaml-value)))
    (:bitvector
     (let* ((bytes (%parse-hex-bytes yaml-value))
            (bits (%unpack-bitvector bytes (getf desc :bits))))
       (ssz:make-vbitvector bits)))
    (:bitlist
     (let* ((bytes (%parse-hex-bytes yaml-value))
            (bits (%unpack-bitlist bytes (getf desc :max-bits))))
       (ssz:make-vbitlist bits)))
    (:vector
     (let ((items (%yaml->list yaml-value)))
       (ssz:make-vvector (mapcar (lambda (v) (%yaml->ssz (getf desc :elem) v)) items))))
    (:list
     (let ((items (%yaml->list yaml-value)))
       (ssz:make-vlist (mapcar (lambda (v) (%yaml->ssz (getf desc :elem) v)) items))))
    (:proglist
     (let ((items (%yaml->list yaml-value)))
       (ssz:make-vlist (mapcar (lambda (v) (%yaml->ssz (getf desc :elem) v)) items))))
    (:progbitlist
     (let* ((bytes (%parse-hex-bytes yaml-value))
            (bits (%unpack-bitlist bytes (* (length bytes) 8))))
       (ssz:make-vbitlist bits)))
    (:container
     (let* ((fields (getf desc :fields))
            (mapping (%mapping->hash yaml-value)))
       (ssz:make-vcontainer
        (mapcar (lambda (field)
                  (let* ((name (car field))
                         (fdesc (cdr field))
                         (value (gethash (%normalize-key name) mapping)))
                    (when (null value)
                      (error "missing field in value.yaml: ~a" name))
                    (%yaml->ssz fdesc value)))
                fields))))
    (:progcontainer
     (let* ((fields (getf desc :fields))
            (mapping (%mapping->hash yaml-value)))
       (ssz:make-vcontainer
        (mapcar (lambda (field)
                  (let* ((name (car field))
                         (fdesc (cdr field))
                         (value (gethash (%normalize-key name) mapping)))
                    (when (null value)
                      (error "missing field in value.yaml: ~a" name))
                    (%yaml->ssz fdesc value)))
                fields))))
    (t (error "yaml conversion not implemented for kind: ~a" (getf desc :kind)))))

(defun %read-root (path)
  (let ((yaml (%yaml-parse-file path)))
    (let* ((root (cond
                   ((and (listp yaml)
                         (assoc "root" yaml :test #'string=))
                    (cdr (assoc "root" yaml :test #'string=)))
                   ((and (listp yaml)
                         (assoc :root yaml))
                    (cdr (assoc :root yaml)))
                   (t (getf yaml :root)))))
      (%parse-hex-bytes root))))

(defun %maybe-snappy-decompress (bytes)
  (let* ((pkg (or (find-package :snappy)
                  (find-package :cl-snappy)
                  (find-package :snappy-java)))
         (sym (and pkg (or (find-symbol "DECOMPRESS" pkg)
                           (find-symbol "UNCOMPRESS" pkg)
                           (find-symbol "DECODE" pkg)))))
    (when (and sym (fboundp sym))
      (funcall (symbol-function sym) bytes))))

(defun %read-file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((len (file-length in)))
      (if len
          (let ((bytes (make-array len :element-type '(unsigned-byte 8))))
            (read-sequence bytes in)
            bytes)
          (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
                (buf (make-array 4096 :element-type '(unsigned-byte 8))))
            (loop for count = (read-sequence buf in)
                  while (> count 0) do
                    (let ((start (fill-pointer out)))
                      (adjust-array out (+ start count))
                      (replace out buf :start1 start :end2 count)))
            out)))))

(defun %read-serialized (path)
  (let* ((bytes (%read-file-bytes path))
         (decoded (%maybe-snappy-decompress bytes)))
    (if decoded
        decoded
        (progn
          (fiveam:skip "snappy not available; skipping serialized compare")
          nil))))

(defun %bytes= (a b)
  (and (= (length a) (length b))
       (loop for i from 0 below (length a) always
         (= (aref a i) (aref b i)))))

(defun %case-type (category case-name)
  (cond
    ((string= category "boolean")
      (list :kind :bool :ssz-type (ssz:make-bool-type)))
    ((string= category "uints")
     (multiple-value-bind (bits _) (%parse-uint-bits case-name "uint_")
       (declare (ignore _))
       (list :kind :uint :bits bits :ssz-type (%make-uint-type bits))))
    ((string= category "bitvector")
     (multiple-value-bind (bits _) (%parse-uint-bits case-name "bitvec_")
       (declare (ignore _))
       (list :kind :bitvector :bits bits :ssz-type (ssz:make-bitvector-type bits))))
    ((string= category "bitlist")
     (multiple-value-bind (bits _) (%parse-uint-bits case-name "bitlist_")
       (declare (ignore _))
       (list :kind :bitlist :max-bits bits :ssz-type (ssz:make-bitlist-type bits))))
    ((string= category "basic_vector")
     (when (%string-prefix-p "vec_" case-name)
       (let* ((rest (subseq case-name 4))
              (sep (position #\_ rest))
              (type-name (subseq rest 0 sep))
              (after (subseq rest (1+ sep))))
         (multiple-value-bind (len _) (%parse-decimal-from after 0)
           (declare (ignore _))
           (let ((elem-desc (%parse-type-desc type-name)))
             (list :kind :vector
                   :elem elem-desc
                   :ssz-type (ssz:make-vector-type (getf elem-desc :ssz-type) len)))))))
    ((string= category "basic_progressive_list")
     (when (%string-prefix-p "proglist_" case-name)
       (let* ((rest (subseq case-name (length "proglist_")))
              (sep (position #\_ rest))
              (type-name (if sep (subseq rest 0 sep) rest)))
         (let ((elem-desc (%parse-type-desc type-name)))
           (list :kind :proglist
                 :elem elem-desc
                 :ssz-type (ssz:make-progressive-list-type (getf elem-desc :ssz-type)))))))
    ((string= category "progressive_bitlist")
     (list :kind :progbitlist :ssz-type (ssz:make-progressive-bitlist-type)))
    ((string= category "containers")
     (when *ssz-generic-schema*
       (let* ((base (let ((pos (position #\_ case-name)))
                      (if pos (subseq case-name 0 pos) case-name)))
              (containers (%schema-containers)))
         (when containers
           (%resolve-container-desc base containers (%schema-progressive-containers))))))
    ((string= category "progressive_containers")
     (when *ssz-generic-schema*
       (let* ((base (let ((pos (position #\_ case-name)))
                      (if pos (subseq case-name 0 pos) case-name)))
              (containers (%schema-containers))
              (progressive (%schema-progressive-containers)))
         (when progressive
           (%resolve-progressive-container-desc base containers progressive))))))
    (t nil))

(defun %collect-cases (root category)
  (let* ((valid-dir (merge-pathnames (format nil "~a/valid/" category) root))
         (dirs (uiop:subdirectories valid-dir)))
    (sort dirs #'string< :key #'namestring)))

(defun %collect-invalid (root category)
  (let* ((invalid-dir (merge-pathnames (format nil "~a/invalid/" category) root)))
    (when (probe-file invalid-dir)
      (sort (uiop:subdirectories invalid-dir) #'string< :key #'namestring))))

(defun %run-case (case-dir category)
  (let* ((case-name (car (last (pathname-directory case-dir))))
         (desc (%case-type category case-name)))
    (when (null desc)
      (fiveam:skip (format nil "unsupported case name: ~a" case-name)))
    (let* ((value-path (merge-pathnames "value.yaml" case-dir))
           (meta-path (merge-pathnames "meta.yaml" case-dir))
           (serialized-path (merge-pathnames "serialized.ssz_snappy" case-dir))
           (yaml (%yaml-parse-file value-path))
           (value (%yaml->ssz desc yaml))
           (type (getf desc :ssz-type))
           (root (%read-root meta-path))
           (computed (ssz:hash-tree-root value type)))
      (fiveam:is (%bytes= computed root))
      (when (probe-file serialized-path)
        (let ((serialized (%read-serialized serialized-path)))
          (when serialized
            (let ((encoded (ssz:encode value type)))
              (fiveam:is (%bytes= encoded serialized)))))))))

(defun %run-invalid (case-dir category)
  (let* ((case-name (car (last (pathname-directory case-dir))))
         (desc (%case-type category case-name)))
    (when (null desc)
      (fiveam:skip (format nil "unsupported invalid case name: ~a" case-name)))
    (let* ((serialized-path (merge-pathnames "serialized.ssz_snappy" case-dir))
           (type (getf desc :ssz-type)))
      (unless (probe-file serialized-path)
        (fiveam:skip (format nil "missing serialized.ssz_snappy: ~a" case-name)))
      (let ((serialized (%read-serialized serialized-path)))
        (when serialized
          (fiveam:signals error
            (ssz:decode serialized type)))))))

(defun run-ssz-generic (root)
  (%load-schema)
  (let ((categories (append
                     '("boolean" "uints" "bitvector" "bitlist" "basic_vector"
                       "basic_progressive_list" "progressive_bitlist")
                     (when *ssz-generic-schema* '("containers" "progressive_containers")))))
    (dolist (category categories)
      (dolist (case-dir (%collect-cases root category))
        (%run-case case-dir category)))))

(defun run-ssz-generic-invalid (root)
  (%load-schema)
  (let ((categories (append
                     '("boolean" "uints" "bitvector" "bitlist" "basic_vector"
                       "basic_progressive_list" "progressive_bitlist")
                     (when *ssz-generic-schema* '("containers" "progressive_containers")))))
    (dolist (category categories)
      (dolist (case-dir (%collect-invalid root category))
        (%run-invalid case-dir category)))))

(test ssz-generic-vectors
  (if (or (null *ssz-generic-root*) (string= *ssz-generic-root* ""))
      (fiveam:skip "SSZ_GENERIC_DIR not set")
      (progn
        (run-ssz-generic *ssz-generic-root*)
        (run-ssz-generic-invalid *ssz-generic-root*))))
