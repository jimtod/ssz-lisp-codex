#!/usr/bin/env bash
set -euo pipefail

if ! command -v sbcl >/dev/null 2>&1; then
  echo "sbcl not found" >&2
  exit 1
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export ASDF_SOURCE_REGISTRY="(:source-registry (:tree \"${ROOT_DIR}\") :inherit-configuration)"

ROOT=""
SCHEMA=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)
      echo "Usage: $0 --root PATH [--schema PATH]"
      echo "  --root    Path to ssz_generic test root (or set SSZ_GENERIC_DIR)"
      echo "  --schema  Path to schema yaml (or set SSZ_GENERIC_SCHEMA)"
      exit 0
      ;;
    --root)
      ROOT="${2:-}"
      shift 2
      ;;
    --schema)
      SCHEMA="${2:-}"
      shift 2
      ;;
    *)
      if [[ -z "$ROOT" ]]; then
        ROOT="$1"
      else
        echo "Unexpected argument: $1" >&2
        exit 2
      fi
      shift
      ;;
  esac
done

ARGS=()
if [[ -n "$ROOT" ]]; then
  ARGS+=(--root "$ROOT")
fi
if [[ -n "$SCHEMA" ]]; then
  ARGS+=(--schema "$SCHEMA")
fi

sbcl --eval "(progn (push #p\"${ROOT_DIR}/coalton/\" asdf:*central-registry*) (push #p\"${ROOT_DIR}/coalton/source-error/\" asdf:*central-registry*))" \
  --load ssz-lisp.asd \
  --eval '(asdf:load-system :ssz-lisp/tests)' \
  --eval '(ssz-cli:main)' \
  --quit -- "${ARGS[@]}"
