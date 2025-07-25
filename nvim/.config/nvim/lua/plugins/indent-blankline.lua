return {
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = {
      indent = { char = "│" },
      scope = { enabled = true },
    },
    config = function(_, opts)
      -- Initial setup
      require("ibl").setup(opts)

      -- Store global state
      vim.g._ibl_enabled = true

      -- Define user command
      vim.api.nvim_create_user_command("ToggleIndentLines", function()
        vim.g._ibl_enabled = not vim.g._ibl_enabled
        if vim.g._ibl_enabled then
          require("ibl").setup(opts)
        else
          require("ibl").setup({ enabled = false })
        end
      end, {})
    end,
  },
}

