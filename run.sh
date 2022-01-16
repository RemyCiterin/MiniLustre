#!/bin/bash 

ocamlbuild main.native -use-ocamlfind -package fstarlib,batteries,unix
#./main.native