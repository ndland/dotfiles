return {
  {
    "esmuellert/codediff.nvim",
    tag = "v2.67.0",
    cmd = "CodeDiff",
    opts = {
      highlights = {
        -- Dracula-only contrast treatment.
        --
        -- Whole-line background uses Dracula's official opaque
        -- current-line fallback. Exact changed text uses opaque
        -- composites of the official Dracula VS Code diff colors:
        --   Green 20% over #282A36 -> #305444
        --   Red   50% over #282A36 -> #944046
        line_insert = "#353747",
        line_delete = "#353747",
        char_insert = "#305444",
        char_delete = "#944046",
      },
    },
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
