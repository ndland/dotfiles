return {
  "folke/snacks.nvim",
  lazy = false,
  opts = {
    ---@class snacks.input.Config
    input = {
      position = "float",
      border = "rounded",
      relative = "editor",
    },
    picker = {
      sources = {
        explorer = {
          auto_close = true,
          matcher = { sort_empty = false, fuzzy = true },
        },
        files = {
          cmd = "fd",
          hidden = true,
        },
      },
    },
    dashboard = {
      enabled = true,
      example = "github",
    },
    terminal = {},
  },
}
