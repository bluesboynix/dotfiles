return {
  {
    "neovim/nvim-lspconfig",
    lazy = false,
    config = function()
      local capabilities = require('cmp_nvim_lsp').default_capabilities()
      local lspconfig = require("lspconfig")

      lspconfig.html.setup({
        capabilities = capabilities
      })

      lspconfig.lua_ls.setup({
        capabilities = capabilities
      })

      lspconfig.gopls.setup({
        capabilities = capabilities,
        settings = {
          gopls = {
            analyses = {
              unusedparams = true,
            },
            staticcheck = true,
            gofumpt = true,
          }
        }
      })

      lspconfig.pylsp.setup({
        settings = {
          pylsp = {
            plugins = {
              pycodestyle = { enabled = false }, -- Disable default linter
              mccabe = { enabled = false },
              black = { enabled = false }, -- We'll use black via null-ls
            },
          },
        },
      })

      lspconfig.yamlls.setup({
        settings = {
          yaml = {
            keyOrdering = false,
            format = {
              enable = true,
            },
            validate = true,
            hover = true,
            completion = true,
          },
        },
      })
    end,
  },
}
