local telescope = require("telescope")

telescope.setup({
  defaults = {
    layout_strategy = "horizontal",

    layout_config = {
      preview_width = 0.55,
    },

    sorting_strategy = "ascending",

    prompt_prefix = "   ",
    selection_caret = "❯ ",
  },
})

local builtin = require("telescope.builtin")
local map = vim.keymap.set

map("n", "<leader>ff", builtin.find_files, {
  desc = "Find files",
})

map("n", "<leader>fg", builtin.live_grep, {
  desc = "Live grep",
})

map("n", "<leader>fb", builtin.buffers, {
  desc = "Buffers",
})

map("n", "<leader>fh", builtin.help_tags, {
  desc = "Help",
})

map("n", "<leader>fr", builtin.oldfiles, {
  desc = "Recent files",
})

map("n", "<leader>fc", builtin.commands, {
  desc = "Commands",
})

map("n", "<leader>fd", builtin.diagnostics, {
  desc = "Diagnostics",
})
