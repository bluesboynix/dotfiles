-- lua/config/keymaps.lua

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Telescope
map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Find Files" })
map("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { desc = "Live Grep" })
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Buffers" })
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { desc = "Help Tags" })
--------------------------------------
-- ToggleTerm
local Terminal = require("toggleterm.terminal").Terminal


local float_term = Terminal:new({
  direction = "float",
  hidden = true,
})

local term_bufnr = nil
local term_winid = nil

local function toggle_vsplit_terminal()
  -- If terminal window is open, close it
  if term_winid and vim.api.nvim_win_is_valid(term_winid) then
    vim.api.nvim_win_close(term_winid, true)
    term_winid = nil
  else
    -- If terminal buffer doesn't exist, create it
    if not term_bufnr or not vim.api.nvim_buf_is_valid(term_bufnr) then
      vim.cmd('vsp')
      vim.cmd('wincmd l')
      vim.cmd('terminal')
      term_bufnr = vim.api.nvim_get_current_buf()
      term_winid = vim.api.nvim_get_current_win()
    else
      -- Otherwise, open the existing terminal buffer in a new vertical split on right
      vim.cmd('vsp')
      vim.cmd('wincmd l')
      vim.api.nvim_set_current_buf(term_bufnr)
      term_winid = vim.api.nvim_get_current_win()
    end
  end
end

local term_bufnr_h = nil

local function toggle_bottom_terminal()
  -- If buffer exists and is visible in a window, close that window
  if term_bufnr_h then
    for _, win in ipairs(vim.api.nvim_list_wins()) do
      if vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == term_bufnr_h then
        vim.api.nvim_win_close(win, true)
        return
      end
    end
  end

  -- Otherwise, open the terminal at the bottom
  if not term_bufnr_h or not vim.api.nvim_buf_is_valid(term_bufnr_h) then
    vim.cmd('botright split')
    vim.cmd('resize 15')
    vim.cmd('terminal')
    term_bufnr_h = vim.api.nvim_get_current_buf()
  else
    vim.cmd('botright split')
    vim.cmd('resize 15')
    vim.api.nvim_set_current_buf(term_bufnr_h)
  end
end

vim.keymap.set('n', '<leader>tt', toggle_bottom_terminal, { silent = true })
map("n", "<leader>tf", function() float_term:toggle() end, { desc = "Toggle Floating Terminal" })
vim.keymap.set('n', '<leader>tv', toggle_vsplit_terminal, { silent = true })
-- Escape to normal mode from terminal
vim.api.nvim_set_keymap('t', '<Esc>', [[<C-\><C-n>]], { noremap = true })
------------------------------------------------------
-- lazygit
vim.keymap.set("n", "<leader>gg", "<cmd>ToggleTerm direction=float<CR>lazygit<CR>", { desc = "Lazygit" })
vim.keymap.set("n", "<leader>gt", "<cmd>TermExec cmd='lazygit' direction=horizontal<CR>", { desc = "Lazygit Terminal" })
----------------------------------------------
-- NvimTree
map("n", "<leader>e", "<cmd>NvimTreeToggle<cr>", { desc = "Toggle NvimTree" })
----------------------------------------------
-- LSP
vim.keymap.set("n", "K", vim.lsp.buf.hover, {})
map("n", "<leader>gd", vim.lsp.buf.definition, { desc = "Go to Definition" })
map("n", "<leader>gr", vim.lsp.buf.references, { desc = "Go to References" })
map("n", "<leader>ca", vim.lsp.buf.code_action, { desc = "Code Action" })
vim.keymap.set('n', '<leader>D', vim.diagnostic.setloclist, { desc = "Open diagnostic list" })
------------------------------------------------------
-- Formatting (none-ls or LSP)
map("n", "<leader>gf", vim.lsp.buf.format, { desc = "Format File" })
------------------------------------------------------------
--- allow ctrl + h,j,k,l to nevigate while in insert mode
vim.keymap.set("i", "<C-h>", "<Left>")
vim.keymap.set("i", "<C-j>", "<Down>")
vim.keymap.set("i", "<C-k>", "<Up>")
vim.keymap.set("i", "<C-l>", "<Right>")
--------------------------------------------
-- bufferline
vim.keymap.set("n", "<Tab>", "<cmd>BufferLineCycleNext<CR>", { desc = "Next Buffer" })
vim.keymap.set("n", "<S-Tab>", "<cmd>BufferLineCyclePrev<CR>", { desc = "Previous Buffer" })
vim.keymap.set('n', '<leader>bd', ':bdelete<CR>', { desc = "Close current buffer" })
----------------------------------------------------------
-- ToggleIndentLines
vim.keymap.set("n", "<leader>ti", "<cmd>ToggleIndentLines<cr>", { desc = "Toggle indent lines" })
--------------------------------------------------------------
-- folding using nvim-ufo
vim.keymap.set("n", "zR", require("ufo").openAllFolds, { desc = "Open all folds" })
vim.keymap.set("n", "zM", require("ufo").closeAllFolds, { desc = "Close all folds" })
