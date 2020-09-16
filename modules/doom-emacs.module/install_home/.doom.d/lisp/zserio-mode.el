;;; zserio-mode.el --- Zserio major mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Johannes Wolf
;;
;; Author: Johannes Wolf
;; Maintainer: Johannes Wolf <mail@johannes-wolf.com>
;; Created: September 10, 2020
;; Modified: September 10, 2020
;; Version: 0.0.1
;; Keywords: zserio
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

(defvar zserio-font-lock-keywords
      (let* (
             (x-keywords '("const" "enum" "bitmask" "struct" "union" "choice"
                           "subtype" "on" "if" "optional" "package" "function"
                           "instantiate" "case" "import" "return"
                           "service" "topic"
                           "sql" "sql_table" "sql_database"))
             (x-types '("int" "int8" "int16" "int32" "int64"
                        "varint" "varint8" "varint16" "varint32" "varint64"
                        "bit" "uint8" "uint16" "uint32" "uint64"
                        "varuint" "varuint8" "varuint16" "varuint32" "varuint64"
                        "float16" "float32" "float64"
                        "bool"
                        "string"
                        "extern"))
             (x-operators '("lengthof" "valueof" "numbits"))
             (x-constants '("true" "false"))
             (x-keyword-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-operators-regexp (regexp-opt x-operators 'words))
             (x-constants-regexp (regexp-opt x-constants 'words)))
        `(
          (,"\"\\.\\*\\?" . font-lock-string-face)
          (,x-keyword-regexp . font-lock-keyword-face)
          (,x-types-regexp . font-lock-type-face)
          (,x-operators-regexp . font-lock-builtin-face)
          (,x-constants-regexp . font-lock-constant-face)
          )
        )
      "List of zserio-mode font-lock keywords.")

;;;###autoload
(define-derived-mode zserio-mode prog-mode "zserio mode"
  "Major mode for editing zserio idl files."
  (modify-syntax-entry ?_ "w" zserio-mode-syntax-table)
  (modify-syntax-entry ?/ ". 124b" zserio-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" zserio-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" zserio-mode-syntax-table)

  (setq font-lock-defaults '((zserio-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zs\\'" . zserio-mode))

(provide 'zserio-mode)
;;; zserio-mode.el ends here
