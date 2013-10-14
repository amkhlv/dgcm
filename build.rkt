#!/usr/bin/racket

#lang at-exp racket

(require (planet esilkensen/yaml) racket/system)

(define conf (call-with-input-file (string->path "REGISTRY.yaml") read-yaml))

(for/list ([f (hash-ref conf "html")])
  (system (string-append "scribble " f ".scrbl")))

(for/list ([f (hash-ref conf "htmls")])
  (system (string-append "scribble --htmls " f ".scrbl")))
