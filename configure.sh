#!/bin/sh
opam install xml-light ocp-build
ocp-build -init -configure

if [ $1 = "--data" ] && [ -d $2 ]; then
    cp -r data/test $2
    cp data/nowork.el $2
fi
