local lspconfig = require("lspconfig")
local mason = require("mason")
local mason_lsp = require("mason-lspconfig")

mason.setup()
mason_lsp.setup({
  ensure_installed = {
    "clangd",        -- c/c++
    "pyright",       -- python
    "tsserver",      -- js/ts
    "html", "cssls", -- web
    "lua_ls",        -- lua (lua-language-server)
    "gopls",         -- go
    "rust_analyzer", -- rust
    "bashls",        -- bash
    "yamlls",        -- yaml
  },
  automatic_installation = true,
})

-- Common default on_attach + capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
-- blink.cmp will later integrate; set snippetCapable etc.
capabilities.textDocument.completion.completionItem.snippetSupport = true

local on_attach = function(client, bufnr)
  -- common keymaps or buffer-local LSP attachments
  local bufmap = function(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, {buffer = bufnr, desc = desc})
  end
  bufmap("n", "gd", vim.lsp.buf.definition, "Goto definition")
  bufmap("n", "gr", vim.lsp.buf.references, "References")
  bufmap("n", "K", vim.lsp.buf.hover, "Hover")
end

-- Generic server setup via mason-lspconfig handlers
mason_lsp.setup_handlers({
  -- default handler
  function(server_name)
    lspconfig[server_name].setup({
      on_attach = on_attach,
      capabilities = capabilities,
    })
  end,
  -- custom for clangd (example: enable offset-encoding if needed)
  ["clangd"] = function()
    lspconfig.clangd.setup({
      on_attach = on_attach,
      capabilities = capabilities,
      cmd = { "clangd", "--background-index" },
    })
  end,
  -- lua specific (sumneko replacement: lua_ls)
  ["lua_ls"] = function()
    lspconfig.lua_ls.setup({
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        Lua = {
          diagnostics = { globals = { "vim" } },
          workspace = { library = vim.api.nvim_get_runtime_file("", true) },
        }
      }
    })
  end,
  -- rust (use rust_analyzer default)
  ["rust_analyzer"] = function()
    lspconfig.rust_analyzer.setup({
      on_attach = on_attach,
      capabilities = capabilities,
    })
  end,
})
