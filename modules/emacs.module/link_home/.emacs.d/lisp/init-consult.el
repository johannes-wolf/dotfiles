;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (setq consult-narrow-key "<"))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (define-key vertico-map "?" #'minibuffer-completion-help)
  (define-key vertico-map (kbd "S-RET") #'minibuffer-force-complete-and-exit)
  (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic flex)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))
	read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t))

(use-package savehist
  :init
  (savehist-mode))

(setq enable-recursive-minibuffers t)

(defun consult-ripgrep-symbol-at-point ()
  (interactive)
  (let* ((caller-dir default-directory)
	 (pr (project-current t))
	 (pr-dir (project-root pr)))
    (if pr-dir
	(consult-ripgrep pr-dir (thing-at-point 'symbol))
      (consult-ripgrep pr-dir (thing-at-point 'symbol)))))

(tyrant-def
  "*"  'consult-ripgrep-symbol-at-point
  "sp" 'consult-ripgrep
  "sg" 'consult-git-grep
  "sl" 'consult-locate)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-."   . embark-act)         ;; pick some comfortable binding
   ("C-;"   . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-consult)
;;; init-consult.el ends here
