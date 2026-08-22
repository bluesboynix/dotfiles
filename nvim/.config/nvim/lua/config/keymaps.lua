local map = vim.keymap.set

-- Better escape
map("i", "jk", "<Esc>", {
  desc = "Escape",
})

-- Save
map("n", "<leader>w", "<cmd>write<CR>", {
  desc = "Save",
})

-- Quit
map("n", "<leader>q", "<cmd>quit<CR>", {
  desc = "Quit",
})

-- Clear search
map("n", "<Esc>", "<cmd>nohlsearch<CR>", {
  desc = "Clear search",
})

-- Window navigation
map("n", "<C-h>", "<C-w>h", {
  desc = "Go left",
})

map("n", "<C-j>", "<C-w>j", {
  desc = "Go down",
})

map("n", "<C-k>", "<C-w>k", {
  desc = "Go up",
})

map("n", "<C-l>", "<C-w>l", {
  desc = "Go right",
})

-- Resize
map("n", "<C-Up>", "<cmd>resize +2<CR>")
map("n", "<C-Down>", "<cmd>resize -2<CR>")
map("n", "<C-Left>", "<cmd>vertical resize -2<CR>")
map("n", "<C-Right>", "<cmd>vertical resize +2<CR>")

-- Move selected lines
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

-- Center screen
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")

-- Open a horizontal terminal split at the bottom
vim.keymap.set('n', '<leader>th', ':botright split | term<CR>',
                { desc = 'Term Horizontal' })

-- Open a vertical terminal split on the right
vim.keymap.set('n', '<leader>tv', ':botright vsplit | term<CR>',
                { desc = 'Term Vertical' })


map("t", "<Esc>", "<C-\\><C-n>", {
  desc = "Exit terminal",
})
