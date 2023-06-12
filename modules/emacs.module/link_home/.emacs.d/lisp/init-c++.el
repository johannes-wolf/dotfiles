(use-package ccls
  :straight (ccls :type git :host github
                  :repo "emacs-lsp/emacs-ccls")
  :init (setq ccls-executable "/usr/bin/ccls")
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp))))

(provide 'init-c++)
