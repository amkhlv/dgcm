(module defs_for-syntax racket
(require (for-syntax (planet amkhlv/bystroTeX/slides_for-syntax)))
(provide defines-syntax-for-formulas)
(define (defines-syntax-for-formulas stx)
  (syntax-case stx ()
    [(_ x)
     (datum->syntax 
      #'x
      `(define-syntax 
         (,#'x y) 
         (bystro-formula-syntax 
          #:autoalign-formula-prefix "f"
          #:manual-formula-prefix    "f"
          #:display-math-prefix      "equation"
          #:size-change-notation     "fsize"
          #:size-increase-notation   "fsize+"
          #:size-restore-notation    "fsize="
          #:max-size-increase        8
          #:max-size-decrease        5
          #:max-vert-adjust          8
          y))
      #'x
      #'x
      #'x)])))
