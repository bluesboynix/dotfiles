return {
  "nvim-treesitter/nvim-treesitter",
  lazy = false,
  build = ":TSUpdate",
  event = { "BufReadPost", "BufNewFile" },
  config = function()
    require'nvim-treesitter'.setup{
      install_dir = vim.fn.stdpath('data') .. '/site'
    }

    require'nvim-treesitter'.install {
      'lua'
    }
  end,
}
