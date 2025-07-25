return {
  "nvimtools/none-ls.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local null_ls = require("null-ls")

    null_ls.setup({
      sources = {
        -- lua
        null_ls.builtins.formatting.stylua,
        -- Go formatters
        null_ls.builtins.formatting.gofmt,
        null_ls.builtins.formatting.gofumpt,
        null_ls.builtins.formatting.goimports,
        -- Go diagnostics
        null_ls.builtins.diagnostics.golangci_lint,
        -- YAML formatter
        null_ls.builtins.formatting.yamlfmt,
        -- YAML linter
        null_ls.builtins.diagnostics.yamllint,
        -- Python formatter
        null_ls.builtins.formatting.black,
        null_ls.builtins.formatting.isort,
        -- Python linter (diagnostics)
        null_ls.builtins.diagnostics.pylint.with({
          args = { "--from-stdin", "%filepath" },
          method = null_ls.methods.DIAGNOSTICS_ON_SAVE, -- reduce noise
        }),
      },
    })
  end,
}
