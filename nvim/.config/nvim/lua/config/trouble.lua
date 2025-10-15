require("trouble").setup({
  icons = true,
  use_diagnostic_signs = true,
})
vim.keymap.set("n", "<leader>xx", "<cmd>TroubleToggle<cr>", { desc = "Toggle Trouble" })
