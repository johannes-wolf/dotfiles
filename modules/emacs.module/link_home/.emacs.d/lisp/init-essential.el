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

; GUI
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

; Editor
(use-package smartparens
  :ensure t
  :hook ((text-mode . smartparens-mode)
	 (prog-mode . smartparens-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init-essential)
;;; init-essential.el ends here
