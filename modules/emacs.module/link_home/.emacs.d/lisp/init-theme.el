;; -*- coding: utf-8; lexical-binding: t; -*-

;;(set-frame-font "Source Code Pro 20" nil t)
;;(load-theme 'modus-operandi t)

(setq nano-font-family-monospaced "Source Code Pro")
(setq nano-font-size 20)

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano)
(menu-bar-mode 0)
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
(setq default-frame-alist '((undecorated . t))) 

(nano-faces)
(nano-theme)
(nano-theme-set-dark)
(nano-refresh-theme)
(nano-modeline)

;;(use-package nano-theme
;;  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
;;  :config
;;  (load-theme 'nano t))
;;
;;(use-package nano-modeline
;;  :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
;;  :config
;;  (nano-modeline))
;;
;;(use-package nano-minibuffer
;;  :straight (nano-minibuffer :type git :host github :repo "rougier/nano-minibuffer")
;;  :config
;;  (nano-minibuffer))
;;
;;(use-package nano-layout
;;  :straight (nano-layout :type git :host github :repo "rougier/nano-layout")
;;  :config
;;  (nano-layout))


(provide 'init-theme)
