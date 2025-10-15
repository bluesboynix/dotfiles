local notify = require("notify")
notify.setup({
  stages = "fade_in_slide_out",
  timeout = 2500,
  background_colour = "#1e2030",
})
vim.notify = notify
