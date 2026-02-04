return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    version = false, -- always use latest
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    keys = {
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find files" },
      { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Live grep" },
      { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Help tags" },
      { "<leader>fo", "<cmd>Telescope oldfiles<cr>", desc = "Recent files" },
    },
    config = function()
      require("telescope").setup({
        defaults = {
          prompt_prefix = "  ",
          selection_caret = " ",
          path_display = { "smart" },
          sorting_strategy = "ascending",
          layout_config = {
            horizontal = {
              prompt_position = "top",
            },
          },
          mappings = {
            i = {
              ["<C-j>"] = "move_selection_next",
              ["<C-k>"] = "move_selection_previous",
              ["<C-q>"] = "send_to_qflist",
            },
          },
        },
      })
    end,
  },
}
