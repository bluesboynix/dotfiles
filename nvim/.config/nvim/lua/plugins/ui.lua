require("tokyonight").setup({
  style = "night",
})

vim.cmd.colorscheme("tokyonight")

require("lualine").setup({
  options = {
    theme = "tokyonight",
    globalstatus = true,
    component_separators = "",
    section_separators = "",
  },
})

require("which-key").setup({},)