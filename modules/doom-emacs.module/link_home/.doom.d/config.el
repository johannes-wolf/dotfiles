;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Johannes Wolf"
      user-mail-address "mail@johannes-wolf.com")

(setq doom-font (font-spec :family "Source Code Pro" :size 24)
      doom-big-font (font-spec :family "Source Code Pro" :size 26)
      doom-variable-pitch-font (font-spec :family "DejaVu Serif" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'wombat)
(setq evil-normal-state-cursor '(box "light blue")
      evil-insert-state-cursor '(bar "red")
      evil-visual-state-cursor '(box "orange"))
;;(setq doom-theme 'doom-zenburn)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Avy
(after! avy
  (setq avy-keys '(?u ?i ?a ?e ?n ?r ?t ?d)))

;; Dired
(after! dired
  (setq dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil
        dired-listing-switches "-alh --group-directories-first"
        dired-ls-F-marks-symlinks nil
        dired-recursive-copies 'always))

(map! :map dired-mode-map
      :n "g x" #'dired-open-xdg)

(use-package! dired-single
  :after dired
  :bind (:map dired-mode-map
         ([remap dired-find-file] . dired-single-buffer)
         ([remap dired-up-directory] . dired-single-up-directory)
         ("M-DEL" . dired-prev-subdir)))

(use-package! dired-open
  :after dired
  :custom (dired-open-extensions '(("mp4" . "mpv"))))

(use-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map
        :n "/" #'dired-narrow-fuzzy))

;; Company
(set-company-backend! 'text-mode
  '(:separate company-yasnippet company-capf))

;; Evil
(setq-default evil-escape-key-sequence "xx")
(map! :i
      "C-j" #'sp-forward-slurp-sexp
      "C-S-j" #'sp-forward-barf-sexp)

;; Projectile
(after! projectile
  (setq projectile-generic-command "fd . -0 --type f --color=never"
        projectile-indexing-method 'native)) ;; Bug with fd < v8.3.0

;; Yas
(setq yas-triggers-in-field t)
(setq yas-snippet-dirs (append yas-snippet-dirs '("~/.doom.d/snippets")))

;; Backup
(setq backup-directory-alist '(("." . ,(expand-file-name (format "%s/emacs/backups/" xdg-data))))
      delete-old-versions -1
      vc-make-backup-files t
      version-control t)

;; Interactive functions
(defun my/systemd-user-run (cmd)
  (interactive)
  (shell-command (concat "systemd-run --user -- " cmd)))

(defun my/xdg-open (path)
  (interactive)
  (when path
    (my/systemd-user-run (concat "xdg-open " path))))

(map! :leader
      (:prefix "g"
       :desc "Run xdg-open on current buffer" "x" (lambda () (interactive) (my/xdg-open (buffer-file-name)))
       :desc "Run xdg-open on current buffers directory" "X" (lambda () (interactive) (my/xdg-open (file-name-directory (buffer-file-name))))))

(after! lsp-mode
  (setq lsp-prefer-capf t)
  (add-to-list 'lsp-language-id-configuration '(org-mode . "org")))

(use-package! lsp-ltex
  :hook ((text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp-deferred)))
         (org-mode . (lambda ()
                       (require 'lsp-ltex)
                       (org-ltex-apply-language)
                       (lsp-deferred))))
  :config
  (defun org-ltex-apply-language ()
    (interactive)
    (let ((doc-lang (org-collect-keywords '("LANGUAGE"))))
      (when doc-lang
        (setq lsp-ltex-language (cadar doc-lang)))))
  :init
  (setq lsp-ltex-language "de-DE"))

(after! flycheck
  (global-flycheck-mode nil))

;; Calculator
(after! calc
  (setq calc-symbolic-mode t
        calc-frac-mode t))

;; Envrc
;(use-package! envrc
;  :bind (:map envrc-mode-map
;         ("C-c e" . 'envrc-command-map))
;  :init
;  (envrc-global-mode))

;; Config
(load! "org.el")
(load! "prog.el")
(load! "mail.el")
