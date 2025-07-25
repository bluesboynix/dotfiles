return {
  "williamboman/mason.nvim",
  lazy = false,
  config = function()
    require("mason").setup()

    -- Auto-install formatters/linters
    local registry = require("mason-registry")

    local ensure_installed = {
      -- LSPs
      "pylsp",
      "yamlls",

      -- Formatters
      "black",
      "isort",
      "stylua",
      "gofumpt",
      "goimports",
      "yamlfmt",

      -- Linters
      "pylint",
      "golangci-lint",
      "yamllint",
    }

    for _, name in ipairs(ensure_installed) do
      local ok, pkg = pcall(registry.get_package, name)
      if ok and not pkg:is_installed() then
        pkg:install()
      end
    end
  end,
}
