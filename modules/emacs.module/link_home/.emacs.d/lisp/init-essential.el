;; -*- coding: utf-8; lexical-binding: t; -*-

(defun my-config-reload ()
  (interactive)
  "Reload init.el"
  (load-file (concat my-emacs-d "/init.el")))

(setq warning-minimum-level 'error)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq indicate-empty-lines t)
(setq delete-by-moving-to-trash t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

; Define C-v for minibuffer paste
(define-key minibuffer-local-map (kbd "C-v") 'yank)

; GUI
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'recentf)
(recentf-mode 1)

(use-package exec-path-from-shell
  :straight t
  :straight (exec-path-from-shell :type git :host github
                                  :repo "purcell/exec-path-from-shell")
  :init (exec-path-from-shell-initialize))

; Editor
(use-package smartparens
  :straight t
  :ensure t
  :hook ((text-mode . smartparens-mode)
	 (prog-mode . smartparens-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1)
(savehist-mode 1)
(save-place-mode 1)

; PDF
(use-package openwith
  :straight t
  :ensure t
  :init
  (setq openwith-associations '(("\\.pdf\\'" "evince" (file))))
  :config
  (openwith-mode t))

; XDG-Open
(defun my/xdg-open ()
  (interactive)
  (when (buffer-file-name)
    (shell-command (concat "xdg-open " (buffer-file-name)))))

; EditorConfig
(use-package editorconfig
  :straight t
  :ensure t
  :config
  (editorconfig-mode 1))

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'init-essential)
;;; init-essential.el ends here
