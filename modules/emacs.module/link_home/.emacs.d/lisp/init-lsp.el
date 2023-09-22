;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package lsp-mode
  :straight (:host github :repo "emacs-lsp/lsp-mode")
  :ensure t
  :hook ((lua-mode    . lsp-defered)
         (python-mode . lsp-defered)
         (js-mode     . lsp-defered)
         (sh-mode     . lsp-defered)
         (org-mode    . lsp-defered)
         (c++-mode    . lsp-defered)
         (c-mode      . lsp-defered))
  :commands (lsp lsp-defered)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-file-watch-threshold 2500)
  :config
  (lsp t))

(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :workspace))

(tyrant-def
  "cd" 'lsp-find-definition
  "cg" 'lsp-find-implementation
  "cf" 'lsp-find-references
  "ca" 'lsp-code-actions-at-point
  "cR" 'lsp-rename)

(use-package consult-lsp
  :straight t
  :after consult
  :after lsp
  :ensure t)

(use-package company
  :straight t
  :ensure t
  :after lsp-mode
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
	      ("<tab>" . company-indent-or-complete-common))
  :custom
  ;(company-backends '(company-yasnippet company-ls company-capf))
  (company-transformers '(company-sort-by-occurrence))
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1)
  :config
  (global-company-mode))

(setq lsp-ltex-mother-tongue "de-DE")
(setq lsp-ltex-language "de-DE")

(provide 'init-lsp)
