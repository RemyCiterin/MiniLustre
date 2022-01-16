#!/bin/bash 

cd _build 
ocamldoc ast.mli -html 
ocamldoc main.ml -html 
ocamldoc utils.ml -html 
ocamldoc obc_ast.mli -html 
ocamldoc lustre2obc.ml -html 
ocamldoc BDD.mli -html 
cd ..