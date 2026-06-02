-- Set leader keys early (required by lazy.nvim and plugins)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Bootstrap lazy.nvim and all plugins (from lazy.lua)
require("lazy")

-- Load your core settings, keymaps, and autocommands
require("config.options")
require("config.keymaps")
require("config.autocmds")
