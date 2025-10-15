-- lua/core/lazy.lua
local plugins = require("plugins")  -- this file will return a table

require("lazy").setup(plugins, {
  dev = { path = "~/projects" }, -- optional for local development
  defaults = { lazy = true, version = false },
  install = { colorscheme = { "catppuccin", "tokyonight" } },
  checker = { enabled = true }, -- automatic update check
})
