#!/bin/bash
FILENAME="${1%%.*}.native"
mkdir -p build
cd build
ln -f ../printlib.ml
ln -f ../$1
ocamlopt -o $FILENAME printlib.ml $1 && ./$FILENAME
