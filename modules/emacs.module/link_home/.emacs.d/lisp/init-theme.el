;; -*- coding: utf-8; lexical-binding: t; -*-

;;(set-frame-font "Source Code Pro 20" nil t)
;;(load-theme 'modus-operandi t)

(straight-use-package
  '(ef-themes :type git :host github :repo "protesilaos/ef-themes"))

(require 'ef-themes)
(setq color-themes '(ef-duo-dark
                     ef-trio-dark
                     ef-autumn
                     ef-cherie
                     ef-deuteranopia-dark
                     ef-tritanopia-dark))

(defun random-color-theme ()
  (interactive)
  (random t)
  (load-theme
    (nth (random (length color-themes)) color-themes) t))

(random-color-theme)

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

(random-color-theme)

(provide 'init-theme)
