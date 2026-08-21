require("nvim-tree").setup({
  view = {
    width = 32,
  },

  renderer = {
    group_empty = true,
  },

  filters = {
    dotfiles = false,
  },

  git = {
    enable = true,
    ignore = false,
  },
})

vim.keymap.set("n", "<leader>e", "<cmd>NvimTreeToggle<CR>", {
  desc = "File tree",
})

vim.keymap.set("n", "<leader>o", "<cmd>NvimTreeFocus<CR>", {
  desc = "Focus file tree",
})
