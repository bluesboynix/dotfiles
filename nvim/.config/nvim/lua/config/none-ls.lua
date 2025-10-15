local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    -- 🧠 General / Web
    null_ls.builtins.formatting.prettier,       -- HTML, JS, CSS, JSON, YAML
    null_ls.builtins.diagnostics.eslint_d,      -- JS/TS linting

    -- 🐍 Python
    null_ls.builtins.formatting.black,
    null_ls.builtins.diagnostics.flake8,

    -- 🦀 Rust
    null_ls.builtins.formatting.rustfmt,

    -- 🦫 Go
    null_ls.builtins.formatting.gofmt,

    -- ⚙️ C / C++
    null_ls.builtins.formatting.clang_format,

    -- 🪶 Lua
    null_ls.builtins.formatting.stylua,

    -- 🐚 Shell
    null_ls.builtins.formatting.shfmt,
    null_ls.builtins.diagnostics.shellcheck,

    -- 📘 YAML
    null_ls.builtins.formatting.yamlfmt,
  },
})

-- Optional: keybinding to format
vim.keymap.set("n", "<leader>f", function()
  vim.lsp.buf.format({ async = true })
end, { desc = "Format buffer (LSP/none-ls)" })

