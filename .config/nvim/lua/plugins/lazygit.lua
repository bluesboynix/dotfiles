return {
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    opts = {},
    config = function()
      require("toggleterm").setup()
      local Terminal = require("toggleterm.terminal").Terminal
      local lazygit = Terminal:new({ cmd = "lazygit", hidden = true, direction = "float" })

      function _LAZYGIT_TOGGLE()
        lazygit:toggle()
      end
    end,
  },
}
