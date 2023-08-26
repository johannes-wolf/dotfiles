;; -*- coding: utf-8; lexical-binding: t; -*-

(straight-use-package
  '(ef-themes :type git :host github :repo "protesilaos/ef-themes"))

(require 'ef-themes)
(setq color-themes '(ef-duo-dark
                     ef-trio-dark
                     ef-autumn
                     ef-cherie
                     ef-deuteranopia-dark
                     ef-tritanopia-dark))
(load-theme (nth 3 color-themes) t)

(cond
 ((find-font (font-spec :name "Source Code Pro"))
  (set-face-attribute 'default nil :font "Source Code Pro-18")))

(use-package doom-modeline
  :straight t
  :ensure t
  :config
  (setq doom-modeline-hud t
	doom-modeline-height 20
	doom-modeline-icon nil)
  :hook (after-init . doom-modeline-mode))

;(straight-use-package
;  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

;(setq nano-font-family-monospaced "Source Code Pro")
;(setq nano-font-size 20)

;(require 'nano)
;(require 'nano-faces)
;(require 'nano-theme)
;(require 'nano-session)
;(require 'nano-modeline)
;(require 'nano-layout)

(menu-bar-mode 0)
(setq default-frame-alist '((undecorated . t)
                            (drag-internal-border . t)
                            (internal-border-width . 5))) 

;(nano-faces)
;(nano-theme)
;(nano-theme-set-dark)
;(nano-refresh-theme)
;(nano-modeline)
;(nano-splash)

(provide 'init-theme)
