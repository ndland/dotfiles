return {
  "catppuccin/nvim",
  lazy = false,
  name = "catppuccin",
  priority = 1000,
  opts = {},
  config = function()
    -- vim.cmd("set termguicolors")
    vim.cmd("colorscheme catppuccin-macchiato")

    local catppuccin = require("catppuccin")

    catppuccin.setup({
      integrations = {
        cmp = true,
        diffview = true,
        gitsigns = true,
        markdown = true,
        snacks = true,
      },
    })
  end,
}
