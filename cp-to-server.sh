#!/bin/bash

cp index.html ~/a/other/server/www-data/scribbles/teaching/gdmc/
cp *.css ~/a/other/server/www-data/scribbles/teaching/gdmc/
rsync -cav --delete manifold/ ~/a/other/server/www-data/scribbles/teaching/gdmc/manifold/
