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

; Emacs
(tyrant-def
  "qq" 'save-buffers-kill-emacs
  "xx" 'execute-extended-command)

; Buffer
(tyrant-def
  "bk" 'kill-this-buffer
  "bs" 'save-buffer
  "bb" 'switch-to-buffer
  "b[" 'evil-prev-buffer
  "b]" 'evil-next-buffer
  "bn" 'evil-buffer-new
  "xh" 'mark-whole-buffer)

; File
(tyrant-def
  "ff" 'find-file
  "fo" 'find-alternate-file
  "fp" 'find-file-at-point
  "fd" 'dired-jump)

; Dired
(tyrant-def
  "dd" 'dired)

; Project
(defun project-find-word-at-point ()
  (interactive)
  (unless (region-active-p)
    (er/mark-word))
  (when (region-active-p)
    (project-find-regexp (buffer-substring (region-beginning) (region-end)))))

(tyrant-def
  "*"   'project-find-word-at-point
  "SPC" 'project-find-file
  "pg"  'project-find-regexp
  "pd"  'project-dired)

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

(use-package expand-region
  :after general
  :commands (er/expand-region
	     er/contract-region)
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region))
  :config
  (tyrant-def
    "v" 'er/expand-region))

(provide 'init-evil)
;;; init-evil.el ends here
