-- Highlight on yank
vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking text",
  group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ higroup = "IncSearch", timeout = 150 })
  end,
})

-- Auto-resize splits when Vim is resized
vim.api.nvim_create_autocmd("VimResized", {
  desc = "Resize splits equally when window is resized",
  group = vim.api.nvim_create_augroup("ResizeSplits", { clear = true }),
  command = "tabdo wincmd =",
})

-- Go to last edited line when opening a buffer (except gitcommit)
vim.api.nvim_create_autocmd("BufReadPost", {
  group = vim.api.nvim_create_augroup("LastPosition", { clear = true }),
  callback = function(event)
    local exclude = { "gitcommit" }
    if vim.tbl_contains(exclude, vim.bo[event.buf].filetype) then return end
    local mark = vim.api.nvim_buf_get_mark(event.buf, '"')
    local lcount = vim.api.nvim_buf_line_count(event.buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Open help in a vertical split
vim.api.nvim_create_autocmd("BufWinEnter", {
  pattern = { "*.txt" },
  group = vim.api.nvim_create_augroup("HelpInVSplit", { clear = true }),
  callback = function()
    if vim.bo.filetype == "help" then
      vim.cmd("wincmd L")  -- move help window to the far right
    end
  end,
})
