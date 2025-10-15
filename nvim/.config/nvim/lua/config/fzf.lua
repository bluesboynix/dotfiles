require("fzf-lua").setup({
  winopts = { height = 0.40 },
  files = { previewer = true },
  git = { files = { cmd = "git ls-files --exclude-standard" } },
  -- tune as you like
})
