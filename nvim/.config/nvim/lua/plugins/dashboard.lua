return {
  {
    "nvimdev/dashboard-nvim",
    event = "VimEnter",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("dashboard").setup({
        center = {
          {
            icon = '',
            icon_hl = 'group',
            desc = 'description',
            desc_hl = 'group',
            key = 'shortcut key in dashboard buffer not keymap !!',
            key_hl = 'group',
            key_format = ' [%s]', -- `%s` will be substituted with value of `key`
            action = '',
          },
        },
        footer = {},
        vertical_center = false, -- Center the Dashboard on the vertical
      })
    end,
  }
}


-- return {
--   {
--     "nvimdev/dashboard-nvim",
--     event = "VimEnter",
--     dependencies = { "nvim-tree/nvim-web-devicons" },
--     config = function()
--       require("dashboard").setup({
--           theme = 'hyper',
--           config = {
--               week_header = {
--                 enable = true,
--               },
--               shortcut = {
--                 {
--                   desc = '󰊳 Update',
--                   group = '@property',
--                   action = 'Lazy update',
--                   key = 'u' },
--                 {
--                   icon = ' ',
--                   icon_hl = '@variable',
--                   desc = 'Files',
--                   group = 'Label',
--                   action = 'Telescope find_files',
--                   key = 'f',
--                 },
--                 {
--                   desc = ' Apps',
--                   group = 'DiagnosticHint',
--                   action = 'Telescope app',
--                   key = 'a',
--                 },
--                 {
--                     desc = ' dotfiles',
--                     group = 'Number',
--                     action = 'Telescope dotfiles',
--                     key = 'd',
--                 },
--               },
--           },
--       })
--     end,
--   },
-- }
--
--
