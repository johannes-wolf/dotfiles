;; -*- coding: utf-8; lexical-binding: t; -*-

;;(set-frame-font "Source Code Pro 20" nil t)
;;(load-theme 'modus-operandi t)

(setq nano-font-family-monospaced "Source Code Pro")
(setq nano-font-size 20)

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano)
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-session)
(require 'nano-modeline)
(require 'nano-layout)

(menu-bar-mode 0)
(setq default-frame-alist '((undecorated . t)
                            (drag-internal-border . t)
                            (internal-border-width . 5))) 

(nano-faces)
(nano-theme)
(nano-theme-set-dark)
(nano-refresh-theme)
(nano-modeline)
(nano-splash)

(provide 'init-theme)
