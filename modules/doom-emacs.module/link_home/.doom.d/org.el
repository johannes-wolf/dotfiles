;;; org.el -*- lexical-binding: t; -*-

(setq org-directory "~/Org"
      org-agenda-files '("~/Org/" "~/Org/Schule/" "~/Org/Schule/E2/"))

;; Attach
(setq org-attach-method 'mv
      org-attach-store-link-p t
      org-attach-id-dir "./.attach/")

;; Export
(setq org-export-with-sub-superscripts t
      org-export-default-language "de")

;; Misc
(setq org-use-property-inheritance t
      org-file-apps '(("\\.png\\'" . "xdg-open %s")
                      ("\\.jpg\\'" . "xdg-open %s")))

(after! ox-latex
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))
  (add-to-list 'org-latex-classes
               '("school"
                 "\\documentclass\{scrartcl\}
   \\usepackage\{parskip\}
   \[DEFAULT-PACKAGES]
   \[PACKAGES]
   \[EXTRA]")))

(use-package! org
  :init
  ;; Capture templates
  (setq org-capture-templates
        '(("h" "Homework" entry (file+headline "~/Org/Schule/homework.org" "Homework")
           "* TODO %? :%^{Subject|deu|ma|en|fra|ges|geo|eth|inf|phy|che|bio}:\nSCHEDULED: %^{Bis}t\n%i\n%a")
          ("t" "Todo" entry (file "~/Org/todo.org")
           "* TODO %?\n%t\n%i\n%a")
          ("n" "Note" entry (file "~/Org/notes.org")
           "* %?\n%t\n%i\n%a")))

  ;; Smash [f12] to search org directory
  (global-set-key [f12] '(lambda () (interactive) (consult-find org-directory)))

  (sp-local-pair '(org-mode) "<<" ">>" :actions '(insert))
  (sp-local-pair '(org-mode) "$" "$" :actions '(insert))
  (sp-local-pair '(org-mode) "$$" "$$" :actions '(insert))
  (sp-local-pair '(org-mode) "~" "~" :actions '(insert)))

(use-package! org-download
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "flameshot gui --raw > %s")
  (org-download-annotate-function (lambda (link) ""))
  (org-download-heading-lvl nil)
  (org-download-delete-image-after-download t)

  :config
  (map! :map org-mode-map
        ("<f5>" (lambda ()
                  (interactive)
                  (let ((dwim-fun (org-collect-keywords '("F5"))))
                    (when dwim-fun
                      (funcall (intern (cadar dwim-fun)))))))))

;(use-package! org-fragtog-mode
;  :hook (org-mode . org-fragtog-mode))

(use-package! anki-editor
  :hook (org-mode . anki-editor-mode)
  :custom
  (anki-editor-create-decks t)
  (anki-editor-org-tags-as-anki-tags t))

(use-package! org-krita
  :hook (org-mode . org-krita-mode))

(use-package! ox-koma-letter
  :after ox-latex
  :init
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))
  (add-to-list 'org-latex-classes
               '("default-koma-letter"
                 "\\documentclass\[%
   parskip=half\]\{scrlttr2\}
   \[DEFAULT-PACKAGES]
   \[PACKAGES]
   \[EXTRA]"))
  :custom
  (org-koma-letter-class-option-file "DIN5008A"))
