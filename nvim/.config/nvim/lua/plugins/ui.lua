require("tokyonight").setup({
  style = "night",
})

require("onedarkpro").setup()

vim.cmd.colorscheme("onedark_dark")

require("lualine").setup({
  options = {
    theme = "onedark",
    globalstatus = true,
    component_separators = "",
    section_separators = "",
  },
})

require("which-key").setup()
