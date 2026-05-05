return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    lazy = false,
    keys = {
      { "<leader>n", "<cmd>Neotree toggle<CR>", desc = "Toggle NeoTree" },
    },
    config = function()
      require("neo-tree").setup({
        window = {
          width = 25,   -- default is 40, set smaller
          -- you can also set a percentage: width = "25%"
        },
      })
    end,
  }
}
