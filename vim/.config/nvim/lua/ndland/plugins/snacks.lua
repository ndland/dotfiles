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
          ignored = true,
          hidden = true,
          -- matcher = { sort_empty = false, fuzzy = true },
          replace_netrw = true, -- Replace netrw with the snacks explorer
        },
        files = {
          cmd = "fd",
          hidden = true,
          exclude = { "node_modules" },
        },
      },
    },
    dashboard = {
      enabled = true,
      example = "github",
    },
  },
}
