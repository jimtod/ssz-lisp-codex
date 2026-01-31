#!/usr/bin/env bash
set -euo pipefail

if ! command -v sbcl >/dev/null 2>&1; then
  echo "sbcl not found" >&2
  exit 1
fi

sbcl --eval "(ql:quickload (list \
  \"alexandria\" \
  \"concrete-syntax-tree\" \
  \"eclector\" \
  \"eclector-concrete-syntax-tree\" \
  \"float-features\" \
  \"fset\" \
  \"named-readtables\" \
  \"trivial-gray-streams\" \
  \"trivial-garbage\" \
  \"ironclad\" \
  \"cl-yaml\" \
  \"snappy\" \
  \"fiveam\"))" \
  --quit
