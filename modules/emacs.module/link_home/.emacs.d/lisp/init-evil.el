;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :ensure t
  :after evil
  :init
  :config
  (general-create-definer tyrant-def
			  :states '(normal insert motion emacs)
			  :keymaps 'override
			  :prefix "SPC"
			  :non-normal-prefix "M-SPC")
  (tyrant-def "" nil))

(general-def universal-argument-map
	     "SPC u" 'universal-argument-more)

(tyrant-def
 "bk" 'kill-this-buffer
 "bs" 'save-buffer
 "bb" 'switch-to-buffer
 "xh" 'mark-whole-buffer)

; File
(tyrant-def
 "ff" 'find-file
 "fo" 'find-alternate-file
 "fp" 'find-file-at-point
 "fd" 'dired-jump)

; Dired
(tyrant-def
  "dd" 'dired
  "dp" 'project-dired)

; Project
(tyrant-def
  "SPC" 'project-find-file
  "sp"  'project-find-regexp)

; Window
(tyrant-def
  "wc" 'delete-window
  "ww" 'other-window
  "w|" 'evil-window-vsplit
  "w-" 'evil-window-split)

; Misc
(tyrant-def
  "hf"  (lambda () (interactive) (consult-find my-emacs-d))
  "hrr" 'my-config-reload)

(provide 'init-evil)
;;; init-evil.el ends here
