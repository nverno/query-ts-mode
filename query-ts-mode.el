;;; query-ts-mode.el --- Tree-sitter support for tree-sitter queries -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/query-ts-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dash "2.18.0"))
;; Created:  12 October 2023
;; Keywords: languages tree-sitter query

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package defines a major mode for tree-sitter query buffers using the
;; tree-sitter parser from https://github.com/nvim-treesitter/tree-sitter-query.
;; 
;; It provides the following features:
;;  - indentation
;;  - font-locking
;;  - structural navigation using tree-sitter objects
;;
;;; Installation:
;;
;; Install the tree-sitter grammar
;;
;;     (add-to-list
;;      'treesit-language-source-alist
;;      '(query "https://github.com/nvim-treesitter/tree-sitter-query"))
;;
;; And call `treesit-install-language-grammar' to complete the installation.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'dash))
(require 'seq)
(require 'treesit)

(declare-function regex-ts-font-lock-rules "regex-ts")
(defvar regex-ts-font-lock-feature-list)

(defcustom query-ts-mode-indent-level 2
  "Number of spaces for each indententation step."
  :group 'query-ts
  :type 'integer
  :safe 'integerp)

(defface query-ts-mode-fieldname-face
  '((t (:inherit font-lock-property-name-face)))
  "Face to highlight fieldnames in `query-ts-mode'."
  :group 'query-ts)

(defface query-ts-mode-node-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face to highlight named nodes in `query-ts-mode'."
  :group 'query-ts)

(defface query-ts-mode-capture-face
  '((t (:inherit font-lock-type-face :slant italic)))
  "Face to highlight captures in `query-ts-mode'."
  :group 'query-ts)

;;; Syntax

(defvar query-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\; "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    (modify-syntax-entry ?#  "'"  table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table in use in Query buffers.")

;;; Indentation

(defvar query-ts-mode--indent-rules
  `((query
     ((parent-is "program") parent 0)
     ((node-is ")") parent 1)
     ((node-is "]") parent-bol 0)
     ((node-is "field_definition") parent-bol query-ts-mode-indent-level)
     ((node-is "capture") parent-bol query-ts-mode-indent-level)
     ((parent-is "list") parent-bol query-ts-mode-indent-level)
     ((parent-is "named_node") parent 1)
     ((node-is "named_node") standalone-parent 1)
     ((parent-is "predicate") parent-bol query-ts-mode-indent-level)
     ((parent-is "grouping") first-sibling 1)
     ((parent-is "parameters") parent-bol 0)
     (no-node parent 0)
     (catch-all parent 0)))
  "Tree-sitter indentation rules for `query-ts-mode'.")

;;; Font-Lock

(defvar query-ts-mode-font-lock-feature-list
  '(( comment)
    ( regexp string function capture)
    ( node property operator escape-sequence number)
    ( bracket delimiter error))
  "`treesit-font-lock-feature-list' for `query-ts-mode'.")

;; (defvar query-ts-mode--font-lock-embedded
;;   (treesit-font-lock-rules))

(defun query-ts-mode-font-lock-rules (&rest _embedded)
  "Create tree-sitter font-lock rules for `query-ts-mode' accounting for
EMBEDDED parsers."
  (treesit-font-lock-rules
   :language 'query
   :feature 'escape-sequence
   ;; :override t
   `((escape_sequence) @font-lock-escape-face)

   :language 'query
   :feature 'regexp
   '((predicate
      name: ((identifier) @_name (:match "\\`\\(?:lua-\\)match\\'" @_name))
      (parameters (string) @font-lock-regexp-face)))

   :language 'query
   :feature 'comment
   :override 'keep
   '([(comment)] @font-lock-comment-face)
      
   :language 'query
   :feature 'string
   :override 'keep
   '((string) @font-lock-string-face
     (anonymous_node name: (_) @font-lock-string-face))

   :language 'query
   :feature 'property
   '((field_definition
      name: (identifier) @query-ts-mode-fieldname-face))

   :language 'query
   :feature 'capture
   '((capture
      "@" @font-lock-misc-punctuation-face
      (identifier) @query-ts-mode-capture-face))
   
   :language 'query
   :feature 'number
   '(((parameters (identifier) @font-lock-number-face)
      (:match "^[-+]?[0-9]+\\(.[0-9]+\\)?\\'" @font-lock-number-face)))

   :language 'query
   :feature 'function
   '((predicate
      "#" @font-lock-operator-face
      name: (_) @font-lock-function-call-face
      type: (_) @font-lock-operator-face))
   
   :language 'query
   :feature 'operator
   '(["?" "*" "+" "."] @font-lock-operator-face
     "!" @font-lock-negation-char-face)
   
   :language 'query
   :feature 'node
   '((named_node (identifier) @query-ts-mode-node-face)
     ["_"] @font-lock-constant-face)

   :language 'query
   :feature 'bracket
   '(["(" ")" "[" "]"] @font-lock-bracket-face)

   :language 'query
   :feature 'delimiter
   '([":"] @font-lock-delimiter-face)
   
   :language 'query
   :feature 'error
   ;; :override t
   '((ERROR) @font-lock-warning-face)))

;;; Embedded comment/regex

(defvar comment-ts-font-lock-settings)
(defvar comment-ts-font-lock-feature-list)
(defvar comment-ts-indent-rules)
(defvar regex-ts--feature-list)
(defvar regex-ts-font-lock-settings)

(defun query-ts-mode--treesit-language-at-point (point)
  "Return the language at POINT."
  (let ((node (treesit-node-at point 'query)))
    (pcase (treesit-node-type node)
      ("comment" 'comment)
      ("\"" (pcase (treesit-node-text
                    (treesit-node-child
                     (treesit-parent-until
                      node (lambda (n) (treesit-node-match-p n "predicate")))
                     0 t))
              ((or "match" "lua-match") 'regex)
              (_ 'query)))
      (_ 'query))))

(defvar query-ts-mode--s-p-query
  (when (treesit-available-p)
    (treesit-query-compile
     'query
     '((predicate
        name: ((identifier) @_name (:match "\\`\\(?:lua-\\)match\\'" @_name))
        (parameters (string) @regex))))))

(defun query-ts-mode--syntax-propertize (start end)
  "Apply syntax text properties between START and END for `query-ts-mode'."
  (let ((captures (treesit-query-capture 'query query-ts-mode--s-p-query start end)))
    (pcase-dolist (`(,name . ,node) captures)
      (when (eq 'regex name)
        (let* ((ns (treesit-node-start node))
               (ne (treesit-node-end node))
               (syntax (string-to-syntax "|")))
          (put-text-property ns (1+ ns) 'syntax-table syntax)
          (put-text-property (1- ne) ne 'syntax-table syntax))))))

(defvar query-ts-mode--treesit-range-rules
  (when (treesit-available-p)
    (treesit-range-rules
     :host 'query
     :embed 'comment
     ;; :offset '(0 . -1)
     '((comment) @comment)
     
     :host 'query
     :embed 'regex
     ;; :local t
     ;; :offset '(1 . -1)
     query-ts-mode--s-p-query))
  "Ranges on which to use embedded parsers.")

;;; Navigation

(defvar query-ts-mode--sentence-nodes nil
  "See `treesit-sentence-type-regexp' for more information.")

(defvar query-ts-mode--sexp-nodes nil
  "See `treesit-sexp-type-regexp' for more information.")

(defvar query-ts-mode--text-nodes
  (rx (or "comment" "string"))
  "See `treesit-text-type-regexp' for more information.")

(defun query-ts--merge-features (a b)
  "Merge `treesit-font-lock-feature-list's A with B."
  (cl-loop for x in a
           for y in b
           collect (seq-uniq (append x y))))

;;;###autoload
(define-derived-mode query-ts-mode prog-mode "Query"
  "Major mode for tree-sitter query buffers.

\\<query-ts-mode-map>"
  :group 'query-ts
  :syntax-table query-ts-mode--syntax-table
  
  ;; Comments
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local parse-sexp-ignore-comments t)

  (when (treesit-ready-p 'query)
    (when (treesit-ready-p 'comment t)
      (treesit-parser-create 'comment))

    (when (treesit-ready-p 'regex t)
      (treesit-parser-create 'regex nil t))

    (treesit-parser-create 'query)
    
    ;; Indentation
    (setq-local treesit-simple-indent-rules query-ts-mode--indent-rules)

    ;; Font-Locking
    (setq-local treesit-font-lock-feature-list query-ts-mode-font-lock-feature-list)
    (setq-local treesit-font-lock-settings (query-ts-mode-font-lock-rules))
    
    ;; Navigation
    (setq-local treesit-defun-tactic 'top-level)
    (setq-local treesit-defun-type-regexp nil)
    
    ;; navigation objects
    (setq-local treesit-thing-settings
                `((query
                   (sexp ,query-ts-mode--sexp-nodes)
                   (sentence ,query-ts-mode--sentence-nodes)
                   (text ,query-ts-mode--text-nodes))))
    
    (let (langs)
      ;; Embedded regex parser
      (when (and (treesit-ready-p 'regex)
                 (require 'regex-ts nil t))
        (push 'regex langs)
        (setq-local treesit-font-lock-settings
                    (append treesit-font-lock-settings
                            (regex-ts-font-lock-rules t)))
        (setq-local treesit-font-lock-feature-list
                    (query-ts--merge-features
                     treesit-font-lock-feature-list
                     regex-ts-font-lock-feature-list)))

      ;; Comment parser
      (when (and (treesit-ready-p 'comment t)
                 (require 'comment-ts nil t))
        (push 'comment langs)
        (setq-local treesit-font-lock-settings
                    (append treesit-font-lock-settings comment-ts-font-lock-settings))
        (setq-local treesit-font-lock-feature-list
                    (query-ts--merge-features
                     treesit-font-lock-feature-list comment-ts-font-lock-feature-list))
        (setq-local treesit-simple-indent-rules
                    (append treesit-simple-indent-rules comment-ts-indent-rules)))
      (when langs
        (setq-local treesit-language-at-point-function
                    #'query-ts-mode--treesit-language-at-point)
        (setq-local treesit-range-settings query-ts-mode--treesit-range-rules)))

    ;; (setq-local treesit-font-lock-settings
    ;;             (append treesit-font-lock-settings query-ts-mode--font-lock-embedded))
    
    (treesit-major-mode-setup)

    (setq-local syntax-propertize-function #'query-ts-mode--syntax-propertize)))

(when (treesit-ready-p 'query)
  (add-to-list 'auto-mode-alist '("\\.query\\'"          . query-ts-mode))
  (add-to-list 'auto-mode-alist '("/queries/.*\\.scm\\'" . query-ts-mode)))

(provide 'query-ts-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; query-ts-mode.el ends here
