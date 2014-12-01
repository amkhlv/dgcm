#!/usr/bin/env racket

#lang at-exp racket

(require (planet esilkensen/yaml) racket/system)

(define conf (call-with-input-file (string->path "REGISTRY.yaml") read-yaml))

(for/list ([f (hash-ref conf "html")])
  (if (hash-has-key? f "dest")
      (copy-file
       (string-append (hash-ref f "dest") "/" (hash-ref f "name") ".html")
       (expand-user-path (string-append 
                          "~/a/other/server/saudade/www-data/scribbles/teaching/gdmc/" 
                          (hash-ref f "dest") 
                          "/"
                          (hash-ref f "name") 
                          ".html"))
       #t)    
      (copy-file 
       (string-append (hash-ref f "name") ".html")
       (expand-user-path (string-append 
                          "~/a/other/server/saudade/www-data/scribbles/teaching/gdmc/" 
                          (hash-ref f "name") 
                          ".html"))
       #t)))
(for/list ([f (hash-ref conf "htmls")])
  (system 
   (string-append 
    "rsync -cav --delete " 
    f
    "/ ~/a/other/server/saudade/www-data/scribbles/teaching/gdmc/"
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
     "~/a/other/server/saudade/www-data/scribbles/teaching/gdmc/" 
     (path->string (file-name-from-path f))))
   #t))

