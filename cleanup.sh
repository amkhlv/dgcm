#!/bin/bash

rm *.png
rm bystrotex.fifo
rm -rf *_formulas/
rm *_formulas.sqlite
rm *.html

for x in manifold exterior-calculus diff-forms vector-fields symplectic-and-poisson-structures ; do
    rm "$x"/*
done
