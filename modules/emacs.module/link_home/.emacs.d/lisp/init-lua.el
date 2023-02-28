(use-package lua-mode
  :ensure t
  :init
  (setq-default lua-indent-level 3
		lua-indent-close-paren-align t
		lua-indent-nested-block-content-align nil))

(use-package fennel-mode
  :ensure t)

(provide 'init-lua)
