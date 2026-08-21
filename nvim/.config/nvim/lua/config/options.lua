local opt = vim.opt

vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- UI
opt.number = true
opt.relativenumber = true
opt.cursorline = true
opt.signcolumn = "yes"
opt.termguicolors = true

-- Editing
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.softtabstop = 2
opt.smartindent = true

-- Search
opt.ignorecase = true
opt.smartcase = true
opt.incsearch = true
opt.hlsearch = true

-- Windows
opt.splitright = true
opt.splitbelow = true

-- Scrolling
opt.scrolloff = 8
opt.sidescrolloff = 8

-- Files
opt.undofile = true
opt.swapfile = false
opt.backup = false

-- Performance
opt.updatetime = 250
opt.timeoutlen = 300

-- Completion
opt.completeopt = {
  "menu",
  "menuone",
  "noselect",
}

-- Appearance
opt.wrap = false
opt.fillchars = {
  eob = " ",
}

opt.list = true
opt.listchars = {
  tab = "→ ",
  trail = "·",
  nbsp = "␣",
}

-- Clipboard
opt.clipboard = "unnamedplus"

-- Mouse
opt.mouse = "a"
