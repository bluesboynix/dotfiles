require("toggleterm").setup({
  size = 15,
  open_mapping = [[<C-\>]],
  shade_terminals = true,
  direction = "float",
  float_opts = { border = "curved" },
})

-- Example keymaps
vim.keymap.set("n", "<leader>tt", "<cmd>ToggleTerm<cr>", { desc = "Toggle terminal" })

