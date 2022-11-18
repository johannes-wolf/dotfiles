;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package eglot
  :hook ((prog-mode-hook . eglot-ensure)))

(use-package consult-eglot
  :ensure t
  :config
  ())

(use-package company
  :ensure t
  :config
  (global-company-mode))

(provide 'init-lsp)
