-- ================================
-- Neovim config 
-- ================================

-- Leader
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- ================================
-- Theme & Transparency
-- ================================
vim.opt.termguicolors = true

local ok, _ = pcall(vim.cmd.colorscheme, "koehler")
if not ok then
  vim.cmd.colorscheme("default")
end

vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
vim.api.nvim_set_hl(0, "NormalNC", { bg = "none" })
vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "none" })

-- ================================
-- UI
-- ================================
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.cursorline = true
vim.opt.wrap = false
vim.opt.scrolloff = 10
vim.opt.sidescrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.showmode = false
vim.opt.cmdheight = 1
vim.opt.pumheight = 10
vim.opt.winblend = 0
vim.opt.pumblend = 10

-- ================================
-- Indentation
-- ================================
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.autoindent = true

-- ================================
-- Search
-- ================================
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = false
vim.opt.incsearch = true

-- ================================
-- Files & Undo
-- ================================
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

local undodir = vim.fn.stdpath("state") .. "/undo"
if not vim.loop.fs_stat(undodir) then
  vim.fn.mkdir(undodir, "p")
end
vim.opt.undofile = true
vim.opt.undodir = undodir

vim.opt.updatetime = 300
vim.opt.timeoutlen = 500
vim.opt.autoread = true

-- ================================
-- Behavior
-- ================================
vim.opt.backspace = "indent,eol,start"
vim.opt.iskeyword:append("-")
vim.opt.path:append("**")
vim.opt.selection = "exclusive"
vim.opt.mouse = "a"
vim.opt.confirm = true
vim.opt.clipboard = "unnamedplus"
vim.opt.encoding = "utf-8"

vim.opt.shortmess:append("c")
vim.opt.formatoptions:remove({ "c", "r", "o" })

-- ================================
-- Cursor
-- ================================
vim.opt.guicursor =
  "n-v-c:block,i-ci-ve:block,r-cr:hor20,o:hor50," ..
  "a:blinkwait700-blinkoff400-blinkon250"

-- ================================
-- Splits
-- ================================
vim.opt.splitbelow = true
vim.opt.splitright = true


-- ================================
-- nvim-treesitter
-- ================================
-- Highlighting
vim.api.nvim_create_autocmd('FileType', {
  pattern = { '<filetype>' },
  callback = function() vim.treesitter.start() end,
})

-- Folds
-- vim.wo[0][0].foldexpr = 'v:lua.vim.treesitter.foldexpr()'
-- vim.wo[0][0].foldmethod = 'expr'

-- Indentation
vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"

require("config.options")
require("config.keymaps")
require("config.autocmds")
require("config.lazy")
