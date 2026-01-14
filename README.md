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