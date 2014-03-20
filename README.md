nominal-workbench
=================

A prototype tool for experimenting nominal rewrite systems

Installing
----------

```
# Only when building the first time
./configure.sh

# Building the project
make

# Testing the project
make test

# Building the documentation
make doc

# Installing nowork
make install

# Uninstalling nowork
make uninstall

# Cleaning files
make clean
```

Installing via OPAM
-------------------

opam repo add nowork git://github.com/pcouderc/nowork-repository
opam install nowork


Informations
------------

Please read the document in doc/, notably the user manual and the developper manual.
