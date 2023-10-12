;;; query-ts-mode.el --- Tree-sitter support for tree-sitter queries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/query-ts-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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

(eval-when-compile (require 'cl-lib))
(require 'treesit)

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
  '((t (:inherit font-lock-type-face :slant normal)))
  "Face to highlight named nodes in `query-ts-mode'."
  :group 'query-ts)

(defface query-ts-mode-capture-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
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
  '((query
     ((parent-is "program") parent 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "field_definition") parent-bol query-ts-mode-indent-level)
     ((parent-is "grouping") first-sibling 1)
     (no-node parent-bol query-ts-mode-indent-level)
     (catch-all parent-bol query-ts-mode-indent-level)))
  "Tree-sitter indentation rules for `query-ts-mode'.")

;;; Font-Lock

(defvar query-ts-mode--feature-list
  '(( comment)
    ( string function capture)
    ( node property operator escape-sequence)
    ( bracket delimiter error))
  "`treesit-font-lock-feature-list' for `query-ts-mode'.")

(defvar query-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'query
   :feature 'comment
   '([(comment)] @font-lock-comment-face)

   :language 'query
   :feature 'string
   '((string) @font-lock-string-face
     (anonymous_node name: (_) @font-lock-string-face))

   :language 'query
   :feature 'escape-sequence
   :override 'prepend
   `((escape_sequence) @font-lock-escape-face)
      
   :language 'query
   :feature 'property
   '((field_definition
      name: (identifier) @query-ts-mode-fieldname-face))

   :language 'query
   :feature 'capture
   '((capture)  @query-ts-mode-capture-face)
   
   :language 'query
   :feature 'function
   '((predicate
      "#" @font-lock-function-call-face
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
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `query-ts-mode'.")

;;; Navigation

(defvar query-ts-mode--sentence-nodes nil
  "See `treesit-sentence-type-regexp' for more information.")

(defvar query-ts-mode--sexp-nodes nil
  "See `treesit-sexp-type-regexp' for more information.")

(defvar query-ts-mode--text-nodes
  (rx (or "comment" "string"))
  "See `treesit-text-type-regexp' for more information.")

;;;###autoload
(define-derived-mode query-ts-mode prog-mode "Query"
  "Major mode for tree-sitter query buffers.

\\<query-ts-mode-map>"
  :group 'query-ts
  :syntax-table query-ts-mode--syntax-table
  (when (treesit-ready-p 'query)
    (treesit-parser-create 'query)

    ;; Comments
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (setq-local comment-start-skip ";+[ \t]*")
    (setq-local parse-sexp-ignore-comments t)

    ;; Indentation
    (setq-local treesit-simple-indent-rules query-ts-mode--indent-rules)

    ;; Font-Locking
    (setq-local treesit-font-lock-feature-list query-ts-mode--feature-list)
    (setq-local treesit-font-lock-settings query-ts-mode--font-lock-settings)
    
    ;; Navigation
    (setq-local treesit-defun-tactic 'top-level)
    (setq-local treesit-defun-type-regexp nil)
    
    ;; navigation objects
    (setq-local treesit-thing-settings
                `((query
                   (sexp ,query-ts-mode--sexp-nodes)
                   (sentence ,query-ts-mode--sentence-nodes)
                   (text ,query-ts-mode--text-nodes))))

    (treesit-major-mode-setup)))

(when (treesit-ready-p 'query)
  (add-to-list 'auto-mode-alist '("\\.query\\'" . query-ts-mode)))

(provide 'query-ts-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; query-ts-mode.el ends here
