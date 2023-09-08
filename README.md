Aperitif
--------

See https://discuss.ocaml.org/t/ann-miou-a-simple-scheduler-for-ocaml-5/12963

Take it for a spin with.

```
opam update
opam sw create . 5.1.0~rc3
opam pin . -yn
opam install . --deps-only
dune build --profile release
./_build/default/src/<eio|miou>/<main|main_p|main_s>.exe ~/.opam/<big-switch>
```