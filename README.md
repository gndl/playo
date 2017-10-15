# Playo
Simple audio player in OCaml


Building and installing Playo
==============================


Compilation
-----------

Dependencies: ocaml >= 4.00.1, ocamlfind, opam, lablgtk2, config-file, ffmpeg

Note that lablgtk2 and config-file will be automatically downloaded at the configure time.

    $ opam pin add ffmpeg https://github.com/ocaml/opam.git#master
    $ ./configure
    $ make


Installation
------------

    $ make install 


