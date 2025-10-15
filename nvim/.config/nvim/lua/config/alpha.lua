local alpha = require("alpha")
local dashboard = require("alpha.themes.dashboard")

dashboard.section.header.val = {
  "███╗   ██╗███████╗██╗   ██╗██╗███╗   ███╗",
  "████╗  ██║██╔════╝██║   ██║██║████╗ ████║",
  "██╔██╗ ██║█████╗  ██║   ██║██║██╔████╔██║",
  "██║╚██╗██║██╔══╝  ╚██╗ ██╔╝██║██║╚██╔╝██║",
  "██║ ╚████║███████╗ ╚████╔╝ ██║██║ ╚═╝ ██║",
  "╚═╝  ╚═══╝╚══════╝  ╚═══╝  ╚═╝╚═╝     ╚═╝",
}
dashboard.section.buttons.val = {
  dashboard.button("f", "  Find file", ":FzfLua files<CR>"),
  dashboard.button("r", "  Recent files", ":FzfLua oldfiles<CR>"),
  dashboard.button("g", "  Live grep", ":FzfLua live_grep<CR>"),
  dashboard.button("t", "  Open terminal", ":ToggleTerm<CR>"),
  dashboard.button("q", "  Quit", ":qa<CR>"),
}
dashboard.section.footer.val = "Welcome to Neovim 0.11+ IDE ✨"
alpha.setup(dashboard.opts)

