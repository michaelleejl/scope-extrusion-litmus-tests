### Scope Extrusion Litmus Tests for Various MetaLanguages


## BER MetaOCaml 

As of 05 June 2025, the scope extrusion check in BER MetaOCaml N513 permits the following programs:
- `continue.ml`       (safe)
- `discard.ml`        (safe)

and rejects the following programs:
- `construct.ml`      (safe)
- `unsafe.ml`         (unsafe)
- `unsafeTopLevel.ml` (unsafe)