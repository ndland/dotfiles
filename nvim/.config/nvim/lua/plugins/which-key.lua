return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      preset = "modern",
      delay = 300,
      notify = false,
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)

      wk.add({
        { "<leader>b", group = "Debug" },
        { "<leader>c", group = "Code" },
        { "<leader>e", group = "Explorer" },
        { "<leader>f", group = "Find" },
        { "<leader>a", group = "AI" },
        { "<leader>g", group = "Git" },
        { "<leader>gv", group = "Git View" },
        { "<leader>gd", group = "GitSigns diffthis" },
        { "<leader>gh", group = "GitHub" },
        { "<leader>l", group = "LSP" },
        { "<leader>n", group = "Notes" },
        { "<leader>r", group = "Run / Test" },
        { "<leader>t", group = "Terminal" },
        { "<leader>u", group = "UI" },
        { "<leader>x", group = "Problems" },
      })
    end,
  },
}
