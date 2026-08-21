vim.pack.add({
  "https://github.com/nvim-telescope/telescope.nvim",
  "https://github.com/nvim-lua/plenary.nvim",

  "https://github.com/nvim-tree/nvim-tree.lua",
  "https://github.com/nvim-tree/nvim-web-devicons",

  "https://github.com/nvim-lualine/lualine.nvim",

  "https://github.com/folke/which-key.nvim",

  "https://github.com/lewis6991/gitsigns.nvim",

  "https://github.com/folke/tokyonight.nvim",
})

require("plugins.telescope")
require("plugins.nvimtree")
require("plugins.ui")
require("plugins.git")
