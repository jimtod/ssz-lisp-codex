(cl:defpackage #:ssz-cli
  (:use #:cl)
  (:export #:main))

(cl:in-package #:ssz-cli)

(defun %usage ()
  (format t "Usage: ssz-generic [--root PATH] [--schema PATH]~%")
  (format t "  --root    Path to ssz_generic test root (or set SSZ_GENERIC_DIR)~%")
  (format t "  --schema  Path to schema yaml (or set SSZ_GENERIC_SCHEMA)~%"))

(defun %parse-args (args)
  (let ((root nil)
        (schema nil))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((or (string= arg "-h") (string= arg "--help"))
           (return (values :help nil nil)))
          ((string= arg "--root")
           (unless args
             (error "--root requires a value"))
           (setf root (pop args)))
          ((string= arg "--schema")
           (unless args
             (error "--schema requires a value"))
           (setf schema (pop args)))
          ((null root)
           (setf root arg))
          (t
           (error "unexpected argument: ~a" arg)))))
    (values :ok root schema)))

(defun %ensure-root (root)
  (let ((val (or root (uiop:getenv "SSZ_GENERIC_DIR"))))
    (when (or (null val) (string= val ""))
      (error "SSZ_GENERIC_DIR not set; pass --root or set env var"))
    val))

(defun main (&rest argv)
  (let* ((args (if argv argv (uiop:command-line-arguments))))
    (multiple-value-bind (status root schema) (%parse-args args)
      (when (eq status :help)
        (%usage)
        (uiop:quit 0))
      (when schema
        (setf ssz-test::*ssz-generic-schema*
              (ssz-test::%yaml-parse-file schema))
        (setf ssz-test::*ssz-generic-type-cache*
              (make-hash-table :test 'equal)))
      (let* ((root-path (%ensure-root root))
             (failures 0)
             (skips 0))
        (flet ((assert-fn (ok msg)
                 (unless ok
                   (incf failures)
                   (format *error-output* "FAIL: ~a~%" (or msg "assertion failed"))))
               (expect-error-fn (thunk msg)
                 (handler-case
                     (progn
                       (funcall thunk)
                       (incf failures)
                       (format *error-output* "FAIL: ~a~%" (or msg "expected error")))
                   (error () t)))
               (skip-fn (msg)
                 (incf skips)
                 (format *error-output* "SKIP: ~a~%" msg)))
          (let ((ssz-test::*ssz-generic-assert* #'assert-fn)
                (ssz-test::*ssz-generic-expect-error* #'expect-error-fn)
                (ssz-test::*ssz-generic-skip* #'skip-fn))
            (ssz-test::run-ssz-generic root-path)
            (ssz-test::run-ssz-generic-invalid root-path)))
        (format t "SSZ generic done: ~d failures, ~d skips~%" failures skips)
        (uiop:quit (if (> failures 0) 1 0))))))
