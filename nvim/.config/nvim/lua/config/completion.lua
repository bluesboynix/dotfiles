local blink = require("blink.cmp")

blink.setup({
  appearance = {
    use_nvim_cmp_as_default = false,
  },
  sources = {
    { name = "lsp" },
    { name = "buffer" },
    { name = "path" },
    { name = "snippets" },
  },
  mapping = {
    ["<C-n>"] = blink.mapping.select_next_item(),
    ["<C-p>"] = blink.mapping.select_prev_item(),
    ["<CR>"]  = blink.mapping.confirm({ select = true }),
    ["<C-Space>"] = blink.mapping.complete(),
  },
})

-- To wire up lsp capabilities with blink, expose the capabilities
-- (we already set snippetSupport earlier in lsp.lua)
