return {
  "sindrets/diffview.nvim",
  config = function()
    require("diffview").setup({
      use_icons = true,
      view = {
        merge_tool = {
          layout = "diff4_mixed",
        },
      },
    })
  end,
}
