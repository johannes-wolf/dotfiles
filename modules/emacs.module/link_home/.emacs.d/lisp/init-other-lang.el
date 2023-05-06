;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package systemd-mode
  :straight (systemd-mode :type git :host github
                          :repo "holomorph/systemd-mode")
  :ensure t)

(provide 'init-other-lang)
