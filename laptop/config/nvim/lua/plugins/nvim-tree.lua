return {
  {
    "kyazdani42/nvim-tree.lua",
    dependencies = {
      "nvim-lua/plenary.nvim",       -- Required dependency
      "nvim-tree/nvim-web-devicons", -- Icons support
    },
    cmd = "NvimTreeToggle",          -- Load lazily
    config = function()
      require("nvim-tree").setup({
        auto_reload_on_write = true, -- Reload the tree when a file changes
        hijack_netrw = true,         -- Hijack netrw (default file manager in Vim)
        update_cwd = true,           -- Update the current working directory
        filters = {
          dotfiles = false,          -- Show dotfiles
        },
        git = {
          enable = true,  -- Show git status (modified, untracked)
          ignore = false, -- Don't ignore files in git
        },
        view = {
          width = 30,    -- Set the width of the file explorer window
          side = "left", -- Position the tree on the left side
        },
        renderer = {
          indent_markers = {
            enable = true,
          }
        }
      })
      -- Auto-open NvimTree when opening a directory (like `nvim .`)
      local function open_nvim_tree(data)
        local directory = vim.fn.isdirectory(data.file) == 1
        if not directory then return end

        vim.cmd.cd(data.file)
        require("nvim-tree.api").tree.open()
      end

      vim.api.nvim_create_autocmd("VimEnter", {
        callback = open_nvim_tree,
      })
    end,
  },
}
