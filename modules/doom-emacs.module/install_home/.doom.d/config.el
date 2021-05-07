;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Code:

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Johannes Wolf"
      user-mail-address "mail@johannes-wolf.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 24)
      doom-big-font (font-spec :family "Source Code Pro Bold" :size 26))
      ;;doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-spacegrey
      doom-spacegrey-brighter-comments t
      doom-spacegrey-padded-modeline t)
;;(load-theme 'acme t)

;; Org
(setq org-directory "~/Documents/"
      org-clock-idle-time 30
      org-clock-persist t
      org-clock-history-length 15
      org-clock-in-resume t
      org-clock-rounding-minutes 5
      org-clock-out-remove-zero-time-clocks nil
      org-clock-out-when-done t)

(defun j/find-worklog ()
  (interactive)
  (counsel-find-file "~/Documents/Worklog/"))

(map! "C-c c o" #'org-clock-out
      "C-c c f" #'j/find-worklog)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type nil)

;; projectile
(defun my-projectile-load-project-config ()
  (interactive)
  (if (file-exists-p (concat (projectile-project-root) "/CMakeLists.txt"))
      (setq projectile-project-compilation-dir "build"
            projectile-project-compilation-cmd "cmake -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=YES .. && cmake --build ."
            projectile-project-test-cmd "ctest --verbose")))

(after! projectile
  (setq projectile-indexing-method 'hybrid)
  (add-hook 'projectile-after-switch-project-hook #'my-projectile-load-project-config)
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(after! lsp
  (setq lsp-file-watch-threshold 10000
        lsp-enable-file-watchers t
        lsp-file-watch-ignored '(".*cache" ".*/build.*" ".*/deploy")))

(after! cc-mode
  (c-add-style
   "johannes" '("doom"
                (indent-tabs-mode . nil)
                (c-basic-offset . 4)
                (c-offsets-alist
                 (innamespace   . 0)
                 (inextern-lang . 0)
                 (case-label    . 0))))
  (setq-default c-default-style "johannes"))


(map! :desc "case transform" :leader :prefix "_"
      :desc "camelCase"   :nv "c" #'string-inflection-lower-camelcase
      :desc "PascalCase"  :nv "p" #'string-inflection-camelcase
      :desc "kebab-case"  :nv "-" #'string-inflection-kebab-case
      :desc "under_score" :nv "_" #'string-inflection-underscore
      :desc "UNDER_SCORE" :nv "u" #'string-inflection-upcase)

(map! :desc "Forward slurp"  :i "C-d" #'sp-forward-slurp-sexp
      :desc "Backward slurp" :i "C-s" #'sp-backward-slurp-sexp
      :desc "Hybrid slurp"   :i "C-q" #'sp-slurp-hybrid-sexp)

(map! :desc "Show killring" :nv "M-p" #'counsel-yank-pop)

(add-load-path! "lisp")
(load! "lisp/ox-koma-letter.el")

(add-to-list 'custom-theme-load-path "themes") 

(defvar private-file-templates-dir
  (expand-file-name "~/.doom.d/templates/")
  "The path to a directory of yasnippet folders to use for file templates.")

(add-to-list 'yas-snippet-dirs 'private-file-templates-dir)
(set-file-template! "\\.letter$" :trigger "__letter" :mode 'org-mode)

;; Mu4e
(add-load-path! "/usr/local/share/emacs/site-lisp/mu4e")
(after! mu4e
  (setq
   mu4e-get-mail-command "mbsync -a"
   mu4e-maildir-shortcuts
   '(("/Inbox"            . ?i)
     ("/Archives/2021"    . ?a)
     ("/Drafts"           . ?d)
     ("/Deleted Messages" . ?t)
     ("/Sent Messages"    . ?s))))

(set-email-account! "johannes@sigabrt.de"
                    '((user-mail-address     . "johannes@sigabrt.de")
                      (user-full-name        . "Johannes Wolf")
                      (smtpmail-smtp-server  . "mail.your-server.de")
                      (smtpmail-smtp-service . 587)
                      (smtpmail-stream-type  . starttls))
                    nil)

;; Evil
(after! evil
  (setq evil-want-fine-undo t
        evil-split-window-below t
        evil-vsplit-window-right t)
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-split-window-below evil-vsplit-window-right)
    (+ivy/switch-buffer)))

(map! :map evil-window-map
      "SPC" #'rotate-layout)

;; Ivy
(after! ivy
  (setq +ivy-buffer-preview t))

;; Company
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

;; Various defaults
(setq-default delete-by-moving-to-trash t
              window-combination-resize t
              x-stretch-cursor t)
(setq undo-limit 80000000
      auto-save-default t
      truncate-string-ellipsis "…")

;; Start up maximised
(if (eq initial-window-system 'x)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
(add-load-path! "lisp")
(load! "lisp/ox-koma-letter.el")
(load! "lisp/zserio-mode.el")

(provide 'config)
;;; config.el ends here
