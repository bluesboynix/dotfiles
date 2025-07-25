return {
  "akinsho/toggleterm.nvim",
  version = "*",
  config = function()
    require("toggleterm").setup({
      size = 20,
      open_mapping = [[<c-\>]], -- default toggle
      hide_numbers = true,
      shade_terminals = true,
      start_in_insert = true,
      insert_mappings = true,
      persist_size = true,
      direction = "float", -- default direction
      close_on_exit = true,
      shell = vim.o.shell,
      float_opts = {
        border = "curved",
        winblend = 3,
      },
    })

    -- Create specific terminals
    local Terminal = require("toggleterm.terminal").Terminal

    local hterm = Terminal:new({ direction = "horizontal", hidden = true })
    local vterm = Terminal:new({ direction = "vertical", hidden = true })
    local fterm = Terminal:new({ direction = "float", hidden = true })
  end,
}
