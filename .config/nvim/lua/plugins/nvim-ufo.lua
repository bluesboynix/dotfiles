return {
  "kevinhwang91/nvim-ufo",
  dependencies = { "kevinhwang91/promise-async" },
  config = function()
    vim.o.foldcolumn = "1" -- Show fold column
    vim.o.foldlevel = 99   -- Prevent folds from being closed initially
    vim.o.foldenable = true

    require("ufo").setup()
  end,
}
