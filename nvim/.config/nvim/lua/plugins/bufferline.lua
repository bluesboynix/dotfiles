return {
  {
    "akinsho/bufferline.nvim",
    version = "*",                   -- use latest stable version
    dependencies = {
      "nvim-tree/nvim-web-devicons", -- for file icons
    },
    config = function()
      require("bufferline").setup({
        options = {
          numbers = "none",
          close_command = "bdelete! %d",       -- closes buffer (without closing window)
          right_mouse_command = "bdelete! %d", -- same on right click
          middle_mouse_command = nil,
          indicator = {
            icon = "", -- this is the indicator on the active buffer
            style = "icon",
          },
          buffer_close_icon = "󰅙",
          modified_icon = "●",
          close_icon = "",
          left_trunc_marker = "",
          right_trunc_marker = "",
          max_name_length = 30,
          max_prefix_length = 15,
          tab_size = 21,
          diagnostics = "nvim_lsp", -- show LSP diagnostics
          offsets = {
            {
              filetype = "NvimTree",
              text = "File Explorer",
              highlight = "Directory",
              text_align = "left",
            },
          },
          show_buffer_icons = true,
          show_buffer_close_icons = true,
          show_close_icon = false,
          show_tab_indicators = true,
          persist_buffer_sort = true,
          separator_style = "slant",
          enforce_regular_tabs = false,
          always_show_bufferline = true,
          sort_by = "id",
        },
      })
    end,
  },
}
