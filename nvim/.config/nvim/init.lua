-- init.lua (minimal bootstrap + module loader)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  -- bootstrap lazy.nvim
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- load core (options / keymaps / lazy plugins)
require("core.lazy")     -- loads plugin list via lazy.nvim
require("core.options")  -- vim.opt and basic settings
require("core.keymaps")  -- your keymaps
