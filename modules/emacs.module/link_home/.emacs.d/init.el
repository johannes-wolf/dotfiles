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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

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
(require-init 'init-typst t)
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
   '(editorconfig quelpa-use-package openwith consult-lsp company-lsp lsp-mode embark-consult embark marginalia consult-selectrum selectrum-prescient selectrum evil-collection undo-fu undo-tree yasnippet maxima gnuplot-mode gnuplot smartparens evil-org orderless magit vertico consult use-package general evil))
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((use-package) (use-package) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
