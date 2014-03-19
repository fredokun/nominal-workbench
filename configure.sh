#!/bin/sh
if [ $1 != "--opam" ] || [ -z $1 ]; then
    echo "OPAM"
    opam install xml-light ocp-build
fi

ocp-build -init -configure
