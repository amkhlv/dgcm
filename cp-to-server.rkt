#!/usr/bin/racket

#lang at-exp racket

(require (planet esilkensen/yaml) racket/system)

(define conf (call-with-input-file (string->path "REGISTRY.yaml") read-yaml))

(for/list ([f (hash-ref conf "html")])
  (copy-file 
   (string-append f ".html")
   (expand-user-path (string-append "~/a/other/server/www-data/scribbles/teaching/gdmc/" f ".html"))
   #t))
(for/list ([f (hash-ref conf "htmls")])
  (system 
   (string-append 
    "rsync -cav --delete " 
    f
    "/ ~/a/other/server/www-data/scribbles/teaching/gdmc/"
    f
    "/")))
(for/list ((f
            (find-files 
             (lambda (p) 
               (and 
                ((length (explode-path p)) . < . 2)
                (regexp-match #px".css" (path->string (file-name-from-path p))))))))
  (copy-file 
   f 
   (expand-user-path 
    (string-append 
     "~/a/other/server/www-data/scribbles/teaching/gdmc/" 
     (path->string (file-name-from-path f))))
   #t))

