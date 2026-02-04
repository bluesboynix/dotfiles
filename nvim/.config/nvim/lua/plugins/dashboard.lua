-- return {
--   {
--     "nvimdev/dashboard-nvim",
--     event = "VimEnter",
--     dependencies = { "nvim-tree/nvim-web-devicons" },
--     config = function()
--       require("dashboard").setup({
--         theme = "hyper",
--         config = {
--             week_header = {
--             enable = true,
--           },
--           shortcut = {
--             { desc = "󰈞 Files", group = "@property", action = "Telescope find_files", key = "f" },
--             { desc = "󰊄 Recent", group = "@property", action = "Telescope oldfiles", key = "r" },
--             { desc = "󰏔 Config", group = "@property", action = "edit ~/.config/nvim/init.lua", key = "c" },
--             { desc = "󰈆 Quit", group = "@property", action = "qa", key = "q" },
--           },
--         },
--       })
--     end,
--   },
-- }
--
--

return {
  {
    "nvimdev/dashboard-nvim",
    event = "VimEnter",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("dashboard").setup({
          theme = 'hyper',
          config = {
              week_header = {
                enable = true,
              },
              shortcut = {
                {
                  desc = '󰊳 Update',
                  group = '@property',
                  action = 'Lazy update',
                  key = 'u' },
                {
                  icon = ' ',
                  icon_hl = '@variable',
                  desc = 'Files',
                  group = 'Label',
                  action = 'Telescope find_files',
                  key = 'f',
                },
                {
                  desc = ' Apps',
                  group = 'DiagnosticHint',
                  action = 'Telescope app',
                  key = 'a',
                },
                {
                    desc = ' dotfiles',
                    group = 'Number',
                    action = 'Telescope dotfiles',
                    key = 'd',
                },
              },
          },
      })
    end,
  },
}


