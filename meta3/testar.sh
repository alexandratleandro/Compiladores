#!/bin/bash

lex jac.l 
yacc -d jac.y
cc -o jac y.tab.c lex.yy.c -g
./jac -s<testar.ja





