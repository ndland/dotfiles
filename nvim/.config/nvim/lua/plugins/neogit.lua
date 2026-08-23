return {
  {
    "NeogitOrg/neogit",
    commit = "5adc81b26232954cd7a90f158aa7844c18fc3165",
    cmd = "Neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "esmuellert/codediff.nvim",
      {
        "m00qek/baleia.nvim",
        commit = "710537ff5cd669c5a76c5f5b6a9169fd9b913d18",
        config = function()
          vim.g.baleia = require("baleia").setup({})
        end,
      },
    },
    keys = {
      {
        "<leader>gg",
        "<cmd>Neogit<cr>",
        desc = "Git status",
      },
    },
    opts = {
      integrations = {
        codediff = true,
        diffview = false,
      },
      diff_viewer = "codediff",
      log_pager = {
        "delta",
        "--no-gitconfig",
        "--color-only",
        "--syntax-theme=Dracula",
      },
    },
  },
}
