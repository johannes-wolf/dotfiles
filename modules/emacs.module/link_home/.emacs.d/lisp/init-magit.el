(use-package magit
  :ensure t
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(tyrant-def
  "gg" 'magit-status
  "gl" 'magit-log-buffer-file)

(provide 'init-magit)
