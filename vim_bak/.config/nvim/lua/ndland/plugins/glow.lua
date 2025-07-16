return {
  "ellisonleao/glow.nvim",
  config = function()
    require("glow").setup({
      style = "dark", -- dark, light, or sepia
      width = 120,
    })
  end,
  cmd = "Glow",
}
