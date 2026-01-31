I want to implemente ethereum ssz serialization methods in coalon language.

Specs refer to: 
- https://ethereum.github.io/consensus-specs/ssz/simple-serialize/ 
- https://github.com/ethereum/consensus-specs/blob/master/ssz/simple-serialize.md
  
You can find the tests in the repo: https://github.com/ethereum/consensus-spec-tests/tree/master/tests/general/phase0/ssz_generic. The tests are genrated in this repo: https://github.com/ethereum/consensus-specs/tree/master/tests/generators


You can refer to SSZ implementations list here:https://github.com/ethereum/consensus-specs/issues/2138. 


I have download the following repos as submodule as a reference
- https://ethereum.github.io/consensus-specs/
- https://github.com/ethereum/consensus-spec-tests
- https://github.com/sigp/ethereum_ssz
- https://github.com/ferranbt/fastssz/
- https://github.com/ethereum/py-ssz
- github.com/coalton-lang/coalton


Now I want to implementate it in coalton language (Coalton is an efficient, statically typed functional programming language that supercharges Common Lisp.), and make it a useful library for future use.

SSZ generic tests will load `ssz_generic_schema.yaml` by default. You can override with `SSZ_GENERIC_SCHEMA`. Snappy support uses `snappy`.

CLI: run generic vectors without FiveAM by calling `(ssz-cli:main)` or `sbcl --load ssz-lisp.asd --eval '(asdf:load-system :ssz-lisp/tests)' --eval '(ssz-cli:main)' --quit -- --root /path/to/ssz_generic`.

Convenience script: `scripts/run-ssz-generic.sh --root /path/to/ssz_generic`.

Bootstrap Quicklisp deps: `scripts/bootstrap-quicklisp-deps.sh`.

Schema notes:
- `union[...]` is supported; each entry is a type, and `none` is allowed.
- Named variants are supported in `union[...]` as `Name: type`.
- YAML union values can be `{selector: N, value: ...}`, `{Name: value}`, or `[selector, value]`. The selector can be a name.
- `compatible_union[...]` is supported with numeric selectors like `compatible_union[1: byte, 2: uint16]`.
- YAML compatible union values can be `{selector: N, value: ...}`, `{N: value}`, or `[selector, value]`.
- `ssz_generic_schema.yaml` includes compatible union examples matching the SSZ generic format docs.
