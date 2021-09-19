;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Code:
(add-load-path! "lisp")
(load! "lisp/env.el")

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

(defun my/base-font-size ()
  (if (< (display-pixel-width) 1920)
      26
    24))

(setq doom-font (font-spec :family "Source Code Pro" :size (my/base-font-size))
      doom-big-font (font-spec :family "Source Code Pro" :size (+ (my/base-font-size) 2))
      doom-variable-pitch-font (font-spec :family "Source Serif 4 Display" :size (my/base-font-size)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-zenburn
      doom-spacegrey-brighter-comments t
      doom-spacegrey-padded-modeline t)
(set-face-attribute 'evil-ex-lazy-highlight nil :box '(:line-width -1))
;;(load-theme 'acme t)

;; Ivy
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

;; Avy
(after! avy
  (setq avy-keys '(?u ?i ?a ?e ?n ?r ?t ?d)))

;; Calc
(setq calc-angle-mode 'rad
      calc-symbolic-mode t)

;; Parens
(sp-local-pair '(python-mode) "f\"" "\"" :actions '(insert))
(sp-local-pair '(c++-mode) "R\"(" ")\"" :actions '(insert))

;; Which key
(setq which-key-idle-delay 0.25)

;; C/C++
(add-hook! 'c-mode-common-hook
  (lambda ()
    (setq comment-start "/* "
          comment-end   " */")))

;; Jump
(map! :desc "Jump forward" :n "C-}" #'better-jumper-jump-forward
      :desc "Jump backward" :n "C-{" #'better-jumper-jump-backward)

;; Org
(use-package! org
  :custom
  (org-attach-method 'cp)
  (org-attach-auto-tag "attach")
  (org-attach-store-link-p t)
  (org-attach-directory "./.attach")
  (org-startup-with-latex-preview t)
  (org-startup-with-inline-images t)
  (org-list-allow-alphabetical t)
  (org-catch-invisible-edits t)
  (org-export-with-sub-superscripts t)
  (org-log-done 'time)
  (org-use-property-inheritance t)
  (org-file-apps '(("\\.png\\'" . "xdg-open %s")
                   ("\\.jpg\\'" . "xdg-open %s")))
  (org-attach-id-dir "./.attach/")

  :init
  ;; Agenda
  (setq org-directory "~/Org"
        org-agenda-files '("~/Org/"))

  (defun my/find-org (dir)
    (interactive)
    (let* ((cands (split-string
                   (shell-command-to-string (concat "find " dir " -iname \"*.org\" -not -ipath \"*/.stversions/*\" ")) "\n" t)))
      (ivy-read "File: " cands
                :action #'find-file
                :caller 'my/find-org)))
  (global-set-key [f12] '(lambda () (interactive) (my/find-org "~/Org")))

  :config
  ;; Faces
  (set-face-attribute 'org-level-1 nil :weight 'bold :height 2.0)
  (set-face-attribute 'org-level-2 nil :weight 'bold :height 1.5)

  ;; Parens
  (sp-local-pair '(org-mode) "<<" ">>" :actions '(insert))
  (sp-local-pair '(org-mode) "$" "$" :actions '(insert))
  (sp-local-pair '(org-mode) "$$" "$$" :actions '(insert))
  (sp-local-pair '(org-mode) "~" "~" :actions '(insert))

  ;; Capture
  (setq org-capture-templates
        '(("h" "Homework" entry (file+headline "~/Org/Schule/homework.org" "Homework")
           "* TODO %?\n%t\nDEADLINE: %^{Bis}t\n%i\n%a")
          ("t" "Todo" entry (file+datetree "~/Org/todo.org" "ToDo")
           "* TODO %?\n%t\n%i\n%a")
          ("n" "Note" entry (file+datetree "~/Org/notes.org" "Notes")
           "* %?\n%t\n%i\n%a"))))

(use-package! org-download
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "flameshot gui --raw > %s")
  (org-download-annotate-function (lambda (link) ""))
  (org-download-heading-lvl nil)
  (org-download-delete-image-after-download t))

(use-package! org-fragtog-mode
  :hook (org-mode . org-fragtog-mode))

(use-package! anki-editor
  :hook (org-mode . anki-editor-mode)
  :custom
  (anki-editor-create-decks t)
  (anki-editor-org-tags-as-anki-tags t))

;; Dired
(after! dired
  (setq dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil
        dired-listing-switches "-alh --group-directories-first"
        dired-ls-F-marks-symlinks nil
        dired-recursive-copies 'always))

(map! :map dired-mode-map
      :n "g x" #'dired-open-xdg)

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

;; Backup
(use-package! files
  :custom
  (backup-directory-alist '(("." . ,(expand-file-name (format "%s/emacs/backups/" xdg-data)))))
  (delete-old-versions -1)
  (vc-make-backup-files t)
  (version-control t))

;; Evil
(map! :map evil-org-mode-map
      :after evil-org
      :n "g <up>" #'org-backward-heading-same-level
      :n "g <down>" #'org-forward-heading-same-level
      :n "g <left>" #'org-up-element
      :n "g <right>" #'org-down-element)

(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
      (:prefix "b"
       :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))

(map! :map org-mode-map
      :nie "M-SPC M-SPC" (cmd! (insert "\u200B"))) ;; Insert zero-width-space using M-SPACE

(use-package! flyspell
  :hook (org-mode . turn-on-flyspell)
  :config
  (ispell-change-dictionary "german"))

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type nil)

;; projectile
(defun my-projectile-load-project-config ()
  (interactive)
  (setq cmakelists-path (concat (projectile-project-root) "/CMakeLists.txt"))
  (if (file-exists-p cmakelists-path)
      (setq projectile-project-compilation-dir "build"
            projectile-project-compilation-cmd "cmake -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=YES .. && cmake --build ."
            projectile-project-test-cmd "ctest --verbose")))

(after! projectile
  (setq projectile-indexing-method 'hybrid)
  (add-hook 'projectile-after-switch-project-hook #'my-projectile-load-project-config)
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(after! lsp
  (setq lsp-file-watch-threshold 100000
        lsp-enable-file-watchers nil
        lsp-file-watch-ignored '(".ccls*" ".*cache" ".*/build.*" ".*/deploy")))

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

(map! :desc "Show killring" :nv "C-p" #'counsel-yank-pop)

(add-load-path! "lisp")
(load! "lisp/ox-koma-letter.el")

(add-to-list 'custom-theme-load-path "themes") 

;; YAS
(setq yas-triggers-in-field t) ;; Enable nested snippets
(defvar private-file-templates-dir
  (expand-file-name "~/.doom.d/templates/")
  "The path to a directory of yasnippet folders to use for file templates.")

(add-to-list 'yas-snippet-dirs 'private-file-templates-dir)

(set-file-template! "\\.letter$" :trigger "__letter" :mode 'org-mode)
(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)

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
;; LSP
(after! lsp
  (setq lsp-enable-file-watchers nil))

;; Evil
(map!
 (:when (not (featurep! :emacs undo +tree))
  :n "C-r" #'undo-fu-only-redo))

(after! evil
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-substitute-global t
        evil-move-cursor-back nil
        evil-kill-on-visual-paste nil)
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-split-window-below evil-vsplit-window-right)
    (+ivy/switch-buffer)))

(map! :map evil-window-map
      "SPC" #'rotate-layout)

;; Company
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

(set-company-backend!
  '(text-mode
    markdown-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

;; Various defaults
(setq-default delete-by-moving-to-trash t
              window-combination-resize t
              x-stretch-cursor t)
(setq undo-limit 80000000
      auto-save-default t
      truncate-string-ellipsis "…")

;; Org f5 export
(use-package! ox-reveal
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (map! :map org-mode-map
        ("<f5>" (lambda ()
                  (interactive)
                  (let ((dwim-fun (org-collect-keywords '("F5"))))
                    (when dwim-fun
                      (funcall (intern (cadar dwim-fun)))))))))

(use-package! vterm-toggle
  :init
  (global-set-key [f2] 'vterm-toggle)
  (global-set-key [C-f2] 'vterm-toggle-cd))

;; HERE are some additional functions/macros that could help you configure Doom:
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
(load! "lisp/ox-koma-letter.el")
(load! "lisp/letter.el")

(provide 'config)
;;; config.el ends here
