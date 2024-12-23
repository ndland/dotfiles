return {
  "roobert/tailwindcss-colorizer-cmp.nvim",
  -- optionally, override the default options:
  config = function()
    require("tailwindcss-colorizer-cmp").setup({
      color_square_width = 2,
      render = "foreground", -- or 'foreground'
      enable_named_colors = true,
      enable_tailwind = true,
    })
  end,
}
