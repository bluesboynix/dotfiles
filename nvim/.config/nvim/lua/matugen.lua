 local M = {}

function M.setup()
  require('base16-colorscheme').setup({
    base00 = '#0e1214',
    base01 = '#192024',
    base02 = '#212b30',
    base03 = '#576267',
    base04 = '#dbe2e6',
    base05 = '#abbcc4',
    base06 = '#abbcc4',
    base07 = '#abbcc4',
    base08 = '#a7cce1',
    base09 = '#e8b8a8',
    base0A = '#faddd2',
    base0B = '#9dd2c0',
    base0C = '#e9ab96',
    base0D = '#96e9cd',
    base0E = '#f2a88c',
    base0F = '#f7cbba',
  })

  local hi = function(group, opts)
    vim.api.nvim_set_hl(0, group, opts)
  end

  -- telescope.nvim
  hi('TelescopeNormal',         { fg = '#abbcc4',          bg = '#0e1214' })
  hi('TelescopeBorder',         { fg = '#576267',             bg = '#0e1214' })
  hi('TelescopePromptNormal',   { fg = '#abbcc4',          bg = '#0e1214' })
  hi('TelescopePromptBorder',   { fg = '#576267',             bg = '#0e1214' })
  hi('TelescopePromptPrefix',   { fg = '#9dd2c0',             bg = '#0e1214' })
  hi('TelescopePromptCounter',  { fg = '#dbe2e6',  bg = '#0e1214' })
  hi('TelescopePromptTitle',    { fg = '#0e1214',             bg = '#9dd2c0' })
  hi('TelescopePreviewTitle',   { fg = '#0e1214',             bg = '#faddd2' })
  hi('TelescopeResultsTitle',   { fg = '#0e1214',             bg = '#e8b8a8' })
  hi('TelescopeSelection',      { fg = '#abbcc4',          bg = '#212b30' })
  hi('TelescopeSelectionCaret', { fg = '#9dd2c0',             bg = '#212b30' })
  hi('TelescopeMatching',       { fg = '#9dd2c0',             bold = true })

  -- mini.pick
  hi('MiniPickNormal',         { fg = '#abbcc4',          bg = '#0e1214' })
  hi('MiniPickBorder',         { fg = '#576267',             bg = '#0e1214' })
  hi('MiniPickPrompt',   { fg = '#abbcc4',          bg = '#0e1214' })
  hi('MiniPickPromptPrefix',   { fg = '#9dd2c0',             bg = '#0e1214' })
  hi('MiniPickBorderText',    { fg = '#0e1214',             bg = '#9dd2c0' })
  hi('MiniPickMatchCurrent',      { fg = '#abbcc4',          bg = '#212b30' })
  hi('MiniPickPromptCaret', { fg = '#9dd2c0',             bg = '#212b30' })
  hi('MiniPickMatchRanges',       { fg = '#9dd2c0',             bold = true })
end

-- Register a signal handler for SIGUSR1 (matugen updates).
-- The handler re-requires this module, which re-runs the code below, so the
-- previous handle is stopped first; otherwise handlers double on every signal.
if _G.__matugen_signal then
  _G.__matugen_signal:stop()
  _G.__matugen_signal:close()
end

local signal = vim.uv.new_signal()
_G.__matugen_signal = signal
signal:start(
  'sigusr1',
  vim.schedule_wrap(function()
    package.loaded['matugen'] = nil
    require('matugen').setup()
  end)
)

return M
