return {
  "ayu-theme/ayu-vim",
  lazy = false,
  priority = 1000,
  opts = {},
  config = function()
    vim.cmd("set termguicolors")
    vim.cmd("let ayucolor='mirage'")
    vim.cmd("colorscheme ayu")
  end,
}
