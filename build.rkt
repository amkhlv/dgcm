#!/usr/bin/racket

#lang at-exp racket

(require (planet esilkensen/yaml) racket/system)

(define conf (call-with-input-file (string->path "REGISTRY.yaml") read-yaml))

(when (hash-has-key? conf "html")
  (for/list ([f (hash-ref conf "html")])
    (if (hash-has-key? f "dest")
        (system (string-append "scribble --dest " (hash-ref f "dest") " " (hash-ref f "name") ".scrbl"))
        (system (string-append "scribble " (hash-ref f "name") ".scrbl")))))

(when (hash-has-key? conf "htmls")
  (for/list ([f (hash-ref conf "htmls")])
    (system (string-append "scribble --htmls " f ".scrbl"))))
