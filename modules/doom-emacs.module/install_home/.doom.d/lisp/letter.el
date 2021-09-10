;;; lisp/letter.el -*- lexical-binding: t; -*-
(use-package! ox-koma-letter
  :init
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))
  (add-to-list 'org-latex-classes
               '("default-koma-letter"
                 "\\documentclass\[%
   parskip=half\]\{scrlttr2\}
   \[DEFAULT-PACKAGES]
   \[PACKAGES]
   \[EXTRA]"))
  :custom
  (org-koma-letter-class-option-file "DIN5008A"))

(provide 'letter)
