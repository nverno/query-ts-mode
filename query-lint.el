;;; query-lint.el --- Lint queries -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/query-ts-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 30 April 2024
;; Keywords:

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
;;; Code:

(defvar query-lint--query
  (when (treesit-available-p)
    (treesit-query-compile
     'query
     '((program [(named_node) (list) (grouping)] @toplevel)
       (named_node
        name: _ @node.named)
       (anonymous_node
        name: _ @node.anonymous)
       (field_definition
        name: (identifier) @field)
       (predicate
        name: (identifier) @predicate.name
        type: (predicate_type) @predicate.type)
       (ERROR) @error))))

(defun query-lint--guess-lang (&optional buffer)
  (when-let ((fname (buffer-file-name buffer)))
    (file-name-nondirectory
     (directory-file-name
      (file-name-directory fname)))))

(defun query-lint ()
  (let ((parsers (treesit-parser-list)))))

(provide 'query-lint)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; query-lint.el ends here
