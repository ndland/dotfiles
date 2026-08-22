return {
  {
    "esmuellert/codediff.nvim",
    tag = "v2.67.0",
    cmd = "CodeDiff",
    opts = {},
    keys = {
      {
        "<leader>gvo",
        "<cmd>CodeDiff<cr>",
        desc = "Open repository review",
      },
      {
        "<leader>gvc",
        function()
          require("codediff.ui.lifecycle.cleanup").close()
        end,
        desc = "Close repository review",
      },
      {
        "<leader>gvf",
        "<cmd>CodeDiff file HEAD<cr>",
        desc = "Review current file",
      },
      {
        "<leader>gvh",
        "<cmd>CodeDiff history %<cr>",
        desc = "Current file history",
      },
      {
        "<leader>gvH",
        "<cmd>CodeDiff history<cr>",
        desc = "Repository history",
      },
    },
  },
}
