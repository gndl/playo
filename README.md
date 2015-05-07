# Playo
A simple audio player in OCaml


Building and installing Playo
==============================


Compilation
-----------

Dependencies: ocaml >= 4.00.1, ocamlfind, opam, unix, threads, str, sndfile, ao, taglib, lablgtk2, config-file

Note that config-file, lablgtk2 and the ocaml bindings of ao and taglib will be automatically downloaded at the configure time.

    $ make all


Installation
------------

If you haven't encountered any error in the previous step, just run:

    $ make install 


