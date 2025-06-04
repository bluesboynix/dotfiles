return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",  -- This ensures parsers are installed
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      require("nvim-treesitter.configs").setup {
        ensure_installed = { "lua", "vim", "bash", "python" },  -- List of parsers to install
        highlight = { enable = true },
        indent = { enable = true },
      }
    end,
  },
}

