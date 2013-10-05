#!/bin/bash

REG="manifold exterior-calculus diff-forms vector-fields symplectic-and-poisson-structures classical-mechanics"

cp index.html ~/a/other/server/www-data/scribbles/teaching/gdmc/
cp todo.html ~/a/other/server/www-data/scribbles/teaching/gdmc/
cp *.css ~/a/other/server/www-data/scribbles/teaching/gdmc/
for x in $REG ; do
    rsync -cav --delete $x/ ~/a/other/server/www-data/scribbles/teaching/gdmc/$x/
done


