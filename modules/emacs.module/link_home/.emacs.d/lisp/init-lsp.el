;; -*- coding: utf-8; lexical-binding: t; -*-

;(use-package eglot
;  :hook ((prog-mode-hook . eglot-ensure)))

;(use-package consult-eglot
;  :ensure t
;  :config
;  )

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package consult-lsp
  :after consult
  :after lsp
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode))

(tyrant-def
  "cd" 'lsp-find-definition
  "cf" 'lsp-find-references
  "cr" 'lsp-rename)

(provide 'init-lsp)
