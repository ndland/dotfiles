return {
  "catppuccin/nvim",
  name = "catppuccin",
  lazy = false,
  priority = 1000,
  opts = {},
  config = function()
    vim.cmd("set termguicolors")
    vim.cmd("colorscheme catppuccin")
  end,
}
