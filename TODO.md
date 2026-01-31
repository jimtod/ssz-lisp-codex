# TODO

- Add container/progressive container schema definitions for SSZ generic tests.
- Current container schema file: `ssz_generic_schema.yaml` (loaded by default; override with `SSZ_GENERIC_SCHEMA`).
- Ensure snappy dependency (CL snappy library) is present for serialized vector comparisons.
- Extend schema parser for nested containers/unions if needed.
- Consider adding CLI runner for SSZ generic vectors.
- Optional: add more invalid-case hardening and tests.

## Schema format (for containers)

Set `SSZ_GENERIC_SCHEMA` to a YAML file like:

containers:
  BitsStruct:
    - name: A
      type: bitlist[6]
    - name: B
      type: bitvector[8]
    - name: C
      type: uint16

Case names like `BitsStruct_lengthy_0` use `BitsStruct` as the schema key.
