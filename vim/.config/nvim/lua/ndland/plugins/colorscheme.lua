return {
  "folke/tokyonight.nvim",
  lazy = false,
  priority = 1000,
  opts = {},
  config = function()
    vim.cmd("set termguicolors")
    vim.cmd("colorscheme tokyonight-storm")
  end,
}
