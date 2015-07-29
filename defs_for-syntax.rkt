(module defs_for-syntax racket
  (require (for-syntax racket/base bystroTeX/slides_for-syntax))
  (provide bystro-def-formula)
   (define-syntax (bystro-def-formula stx)
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
      stx)))
