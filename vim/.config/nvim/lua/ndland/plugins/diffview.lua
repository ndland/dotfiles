return {
  "sindrets/diffview.nvim",
  config = function()
    require("diffview").setup({
      view = {
        x = {
          layout = {
            type = "diff4_mixed",
          },
        },
      },
    })
  end,
}
