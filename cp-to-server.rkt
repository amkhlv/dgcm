#!/usr/bin/env racket

#lang racket

(require bystroTeX/utils truques/xml)

(define server-dir (expand-user-path "~/a/other/server/saudade/www-data/scribbles/teaching/gdmc/"))

(define x (file->xexpr "bystrotex.xml"))

(for ([s (se-path*/list '(scribblings) x)] #:when (cons? s))
  (let* ([multipage? (cons?    (filter   (λ  (x)  (equal? x '(multipage ())))   s))]
         [name (let ([v (se-path* '(name) s)]) (if v (string-trim v) #f))]
         [dest (let ([v (se-path* '(dest) s)]) (if v (string-trim v) #f))])
    (if multipage?
        (begin
          (display "Copying multipage: ")
          (displayln s)
          (with-external-command-as 
           cpmp 
           #:cmdline (list "rsync" 
                           "-cav" 
                           "--delete" 
                           (string-append name "/")
                           (path->string (build-path server-dir name)))
           (for ([ln (in-lines cpmp-stdout)]) (displayln ln))))
        (if dest 
            (begin
              (display "Copying dest: ")
              (displayln dest)
              (with-external-command-as
               cpdest
               #:cmdline (list "rsync"
                               "-cav"
                               "--delete"
                               (string-append dest "/")
                               (path->string (build-path server-dir dest)))
               (for ([ln (in-lines cpdest-stdout)]) (displayln ln))))
            (begin
              (display "Copying: ")
              (displayln name)
              (with-external-command-as
               cpsingle
               #:cmdline (list "cp" (string-append name ".html") (path->string (build-path server-dir name)))
               (for ([ln (in-lines cpsingle-stdout)]) (displayln ln))))))))
(let 
    ([ip (run-pipeline
          #f
          #f
          ("find" "." "-maxdepth" "1" "-name" "*.css")
          ("cpio" "-pmad" (path->string (build-path server-dir))))])
  (for ([ln (in-lines ip)]) (displayln ln))
  (close-input-port ip))
            
      
