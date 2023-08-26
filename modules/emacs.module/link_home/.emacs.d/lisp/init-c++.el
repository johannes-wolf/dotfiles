(require 'lsp-mode)

(defun my-c-mode-hook ()
  (setq c-basic-offset 4
        c-indent-level 4
        tab-width 4
        indent-tabs-mode nil)
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(provide 'init-c++)
