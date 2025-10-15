require("noice").setup({
  cmdline = {
    view = "cmdline_popup",
  },
  messages = {
    enabled = true,
  },
  lsp = {
    progress = { enabled = true },
    hover = { enabled = true },
    signature = { enabled = true },
  },
  presets = {
    bottom_search = false,
    command_palette = true,
    long_message_to_split = true,
    inc_rename = true,
    lsp_doc_border = true,
  },
})
