return {
  {
    "mrjones2014/smart-splits.nvim",
    config = function()
      require("smart-splits").setup({
        ignored_filetypes = { "nofile", "quickfix", "prompt", "terminal" },
        resize_mode = {
          silent = true,
          hooks = {
            on_enter = function() print("Entered resize mode") end,
            on_leave = function() print("Left resize mode") end,
          },
        },
      })

      -- Keymaps for resizing windows
      local keymap = vim.keymap.set
      local opts = { noremap = true, silent = true }

      keymap("n", "<A-h>", require("smart-splits").resize_left, opts)
      keymap("n", "<A-j>", require("smart-splits").resize_down, opts)
      keymap("n", "<A-k>", require("smart-splits").resize_up, opts)
      keymap("n", "<A-l>", require("smart-splits").resize_right, opts)

      -- Keymaps for moving between windows
      keymap("n", "<C-h>", require("smart-splits").move_cursor_left, opts)
      keymap("n", "<C-j>", require("smart-splits").move_cursor_down, opts)
      keymap("n", "<C-k>", require("smart-splits").move_cursor_up, opts)
      keymap("n", "<C-l>", require("smart-splits").move_cursor_right, opts)
    end,
  }
}

