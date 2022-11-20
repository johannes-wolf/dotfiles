;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

(defconst my-emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d.")

(defconst my-site-lisp-dir (concat my-emacs-d "site-lisp")
  "Directory of site-lisp.")

(defconst my-lisp-dir (concat my-emacs-d "lisp")
  "Directory of personal configuration.")

(defconst my-lightweight-mode-p nil
  "Lightweight mode")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path my-lisp-dir)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not my-lightweight-mode-p))
    (load (file-truename (format "%s/%s" my-lisp-dir pkg)) t t)))

(require-init 'init-essential)
(require-init 'init-evil)
(require-init 'init-consult)
(require-init 'init-magit)
(require-init 'init-lsp t)
(require-init 'init-theme t)
(require-init 'init-lua t)
(require-init 'init-latex t)
(require-init 'init-org t)
(require-init 'init-math t)
(require-init 'init-snippets t)
(require-init 'init-abbrev t)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult-lsp company-lsp lsp-mode embark-consult embark marginalia consult-selectrum selectrum-prescient selectrum evil-collection undo-fu undo-tree yasnippet maxima gnuplot-mode gnuplot smartparens evil-org orderless magit vertico consult use-package general evil))
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
