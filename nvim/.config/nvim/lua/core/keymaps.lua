-- leader
vim.g.mapleader = " "
local map = vim.keymap.set
map("n", "<leader>ff", "<cmd>FzfLua files<cr>", {desc="Find files"})
map("n", "<leader>fg", "<cmd>FzfLua live_grep<cr>", {desc="Grep"})
map("n", "<leader>fb", "<cmd>FzfLua buffers<cr>", {desc="Buffers"})
map("n", "<leader>ld", "<cmd>lua vim.lsp.buf.definition()<cr>", {desc="LSP goto def"})
map("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<cr>", {desc="LSP rename"})
vim.keymap.set("n", "<leader>e", ":Neotree toggle<CR>", { desc = "Toggle file explorer" })
