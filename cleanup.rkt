#!/usr/bin/racket

#lang at-exp racket

(require (planet esilkensen/yaml) racket/system)

(define conf (call-with-input-file (string->path "REGISTRY.yaml") read-yaml))

(for/list ([f (hash-ref conf "html")])
  (let ((f-html (string->path (string-append f ".html")))
        (f-form (string->path (string-append f "_formulas")))
        (f-sql  (string->path (string-append f "_formulas.sqlite"))))
    (when (file-exists? f-html) (delete-file f-html))
    (when (directory-exists? f-form) (delete-directory/files f-form))
    (when (file-exists? f-sql) (delete-file f-sql))))
(for/list ([f (hash-ref conf "htmls")])
  (let ((f-dir (string->path f))
        (f-form (string->path (string-append f "_formulas")))
        (f-sql (string->path (string-append f "_formulas.sqlite"))))
    (when (directory-exists? f-dir) (delete-directory/files f-dir))
    (when (directory-exists? f-form) (delete-directory/files f-form))
    (when (file-exists? f-sql) (delete-file f-sql))))
(for/list ([f
            (find-files 
             (lambda (p)
               (and 
                ((length (explode-path p)) . < . 2)
                (regexp-match #px"^\\d+\\.png" (path->string (file-name-from-path p))))
               ))])
  (delete-file f)
  )
(let ((bp (string->path "bystrotex.fifo")))
  (when (file-exists? bp) (delete-file bp)))
