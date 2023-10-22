#!/usr/bin/env bash

DIR="$(cd "$( dirname "${BASH_SOURCE[0]}")" && pwd)"
NVIM_REPO=https://github.com/nvim-treesitter

nvim_parsers() {
    local d="${1:-$DIR/nvim-treesitter}"
    ! [ -f "$d" ] && git clone "$NVIM_REPO" "$d"
}
