require("config.options")
require("config.keymaps")
require("config.autocmds")
require("config.plugins")
--local ok, matugen = pcall(require, 'matugen')
if ok then matugen.setup() end
