(setq-default abbrev-mode t) 

(use-package abbrev
  :ensure nil
  :init
  (setq save-abbrevs 'silent))

(provide 'init-abbrev)
