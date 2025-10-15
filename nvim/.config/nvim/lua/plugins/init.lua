return {
  -- core utilities
  { "nvim-lua/plenary.nvim", lazy = true },
  { "nvim-tree/nvim-web-devicons", lazy = true },

  -- main system
  { "folke/snacks.nvim", config = function() require("config.snacks") end },
  { "ibhagwan/fzf-lua", cmd = "FzfLua", config = function() require("config.fzf") end },
  { "Saghen/blink.cmp", event = "InsertEnter", config = function() require("config.completion") end },
  { "williamboman/mason.nvim", cmd = "Mason", config = true },
  { "williamboman/mason-lspconfig.nvim", event = "VeryLazy" },
  { "neovim/nvim-lspconfig", event = {"BufReadPre","BufNewFile"}, config = function() require("config.lsp") end },
  { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate", event = {"BufReadPre","BufNewFile"}, config = function() require("config.treesitter") end },

  -- UI enhancements
  { "bluz71/vim-moonfly-colors", name = "moonfly", lazy = false, priority = 1000, config = function() vim.cmd.colorscheme("moonfly") end },
  { "rcarriga/nvim-notify", config = function() require("config.notify") end },
  { "folke/noice.nvim", event = "VeryLazy", dependencies = { "rcarriga/nvim-notify", "MunifTanjim/nui.nvim" }, config = function() require("config.noice") end },
  { "nvim-lualine/lualine.nvim", event = "VeryLazy" },
  { "akinsho/bufferline.nvim", dependencies = "nvim-web-devicons", event = "BufWinEnter" },

  -- Editing helpers
  { "windwp/nvim-autopairs", event = "InsertEnter", config = function() require("nvim-autopairs").setup({}) end },
  { "numToStr/Comment.nvim", keys = { "gc", "gb" }, config = function() require("Comment").setup() end },
  { "kylechui/nvim-surround", event = "VeryLazy", config = function() require("nvim-surround").setup({}) end },

  -- LSP / diagnostics extras
  { "folke/trouble.nvim", cmd = { "TroubleToggle", "Trouble" }, config = function() require("config.trouble") end },

  -- Keybinding help
  { "folke/which-key.nvim", event = "VeryLazy", config = function() require("which-key").setup({}) end },

  -- Terminal
  { "akinsho/toggleterm.nvim", version = "*", cmd = { "ToggleTerm", "TermExec" }, config = function() require("config.toggleterm") end },

  -- Dashboard
  { "goolord/alpha-nvim", event = "VimEnter", config = function() require("config.alpha") end },

  -- None-ls
  {
    "nvimtools/none-ls.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("config.none-ls")
    end,
  },

  -- Neo-Tree
  {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons",
    "MunifTanjim/nui.nvim",
  },
  event = "VeryLazy",  -- lazy-load when idle
  config = function()
    require("config.neo-tree")
    end,
  },

}
