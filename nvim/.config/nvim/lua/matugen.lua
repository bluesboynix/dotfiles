local M = {}

function M.setup(palette)
  -- Fallback default palette if none is passed
  local colors = palette or {
    base00 = "#1e1e2e", base01 = "#181825", base02 = "#313244", base03 = "#45475a",
    base04 = "#585b70", base05 = "#cdd6f4", base06 = "#f5e0dc", base07 = "#b4befe",
    base08 = "#f38ba8", base09 = "#fab387", base0A = "#f9e2af", base0B = "#a6e3a1",
    base0C = "#94e2d5", base0D = "#89b4fa", base0E = "#cba6f7", base0F = "#f2cdcd"
  }

  -- Set truecolor support
  vim.opt.termguicolors = true

  -- Clear previous styling
  vim.cmd("highlight clear")
  if vim.fn.exists("syntax_on") then vim.cmd("syntax reset") end
  vim.g.colors_name = "matugen_base16"

  -- Helper function to set highlights easily
  local function hi(group, settings)
    vim.api.nvim_set_hl(0, group, settings)
  end

  -- Map base16 colors to basic vim elements
  hi("Normal",       { fg = colors.base05, bg = colors.base00 })
  hi("CursorLine",   { bg = colors.base01 })
  hi("Visual",       { bg = colors.base02 })
  hi("Comment",      { fg = colors.base03, italic = true })
  hi("Constant",     { fg = colors.base09 })
  hi("String",       { fg = colors.base0B })
  hi("Identifier",   { fg = colors.base08 })
  hi("Function",     { fg = colors.base0D })
  hi("Statement",    { fg = colors.base0E })
  hi("PreProc",      { fg = colors.base0A })
  hi("Type",         { fg = colors.base0A })
  hi("Special",      { fg = colors.base0C })
  hi("Error",        { fg = colors.base05, bg = colors.base08 })
  hi("Todo",         { fg = colors.base0A, bg = colors.base01 })
  hi("LineNr",       { fg = colors.base03, bg = colors.base00 })
  hi("CursorLineNr", { fg = colors.base04 })
  
  -- Treesitter elements
  hi("@variable",    { fg = colors.base05 })
  hi("@keyword",     { fg = colors.base0E })
  hi("@function",    { fg = colors.base0D })
end

return M

