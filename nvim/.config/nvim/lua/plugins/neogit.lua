return {
  {
    "NeogitOrg/neogit",
    commit = "5adc81b26232954cd7a90f158aa7844c18fc3165",
    cmd = "Neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "esmuellert/codediff.nvim",
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
    },
  },
}
