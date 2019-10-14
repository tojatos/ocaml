#!/bin/bash
FILENAME="${1%%.*}.native"
ocamlbuild ${FILENAME} && ./${FILENAME}
