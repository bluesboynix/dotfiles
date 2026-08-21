-- Start Tree-sitter automatically when a parser is available.

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local map = vim.keymap.set
    local opts = {
      buffer = args.buf,
      silent = true,
    }

    map("n", "gd", vim.lsp.buf.definition, {
      buffer = args.buf,
      desc = "Go to definition",
    })

    map("n", "gD", vim.lsp.buf.declaration, {
      buffer = args.buf,
      desc = "Go to declaration",
    })

    map("n", "gr", vim.lsp.buf.references, {
      buffer = args.buf,
      desc = "References",
    })

    map("n", "gi", vim.lsp.buf.implementation, {
      buffer = args.buf,
      desc = "Implementation",
    })

    map("n", "K", vim.lsp.buf.hover, {
      buffer = args.buf,
      desc = "Hover",
    })

    map("n", "<leader>rn", vim.lsp.buf.rename, {
      buffer = args.buf,
      desc = "Rename",
    })

    map({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, {
      buffer = args.buf,
      desc = "Code action",
    })

    map("n", "<leader>lf", function()
      vim.lsp.buf.format({
        async = true,
      })
    end, {
      buffer = args.buf,
      desc = "Format",
    })
  end,
})

