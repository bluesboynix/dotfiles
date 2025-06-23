return {
  {
    "goolord/alpha-nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" }, -- optional for icons
    config = function()
      local alpha = require("alpha")
      local dashboard = require("alpha.themes.dashboard")

      -- Set header (ASCII art)
      dashboard.section.header.val = {
        "NEOVIM",
      }

      -- Buttons
      dashboard.section.buttons.val = {
        dashboard.button("f", "  Find file", ":Telescope find_files <CR>"),
        dashboard.button("n", "  New file", ":ene <BAR> startinsert <CR>"),
        dashboard.button("r", "  Recent files", ":Telescope oldfiles <CR>"),
        dashboard.button("g", "  Find text", ":Telescope live_grep <CR>"),
        dashboard.button("c", "  Configuration", ":e $MYVIMRC <CR>"),
        dashboard.button("q", "  Quit NVIM", ":qa<CR>"),
      }

      -- Footer
      dashboard.section.footer.val = "The more you type, the faster you type."
      -- Thank you typecraft for the tutorials.

      alpha.setup(dashboard.config)

      -- Optional: Disable statusline on dashboard
      vim.cmd([[
        autocmd User AlphaReady set showtabline=0 | set laststatus=0
        autocmd User AlphaClosed set showtabline=2 | set laststatus=2
      ]])
    end,
  },
}

