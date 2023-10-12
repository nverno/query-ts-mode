# Tree-sitter query major mode using tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This package provides a major-mode for tree-sitter queries (nvim) using the tree-sitter 
grammar from https://github.com/nvim-treesitter/tree-sitter-query

Features:
- indentation
- font-locking
- structural navigation with treesitter objects

![example](doc/example-query.png)

## Installing

Emacs 29.1 or above with tree-sitter support is required. 

### Install tree-sitter parser for query

Add the source to `treesit-language-source-alist`. 

```elisp
(add-to-list
 'treesit-language-source-alist
 '(query "https://github.com/nvim-treesitter/tree-sitter-query"))
```

Then run `M-x treesit-install-language-grammar` and select `query` to install.
