require('nvim-treesitter.configs').setup {
  ensure_installed = { "c", "cpp", "python", "javascript", "html", "css", "lua", "go", "rust", "bash", "yaml" },
  highlight = { enable = true },
  indent = { enable = true },
  playground = { enable = false },
}
