;; typst-mode is full of bugs atm
;(use-package typst-mode
;  :straight (typst-mode :type git :host github :repo "Ziqi-Yang/typst-mode.el"))

(define-derived-mode typst-mode
  prog-mode "Typst"
  "Major mode for typst.")

(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-mode))

(provide 'init-typst)
