return {
  "nvim-lualine/lualine.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    "RRethy/base16-nvim",
  },

  config = function()
    local lualine = require("lualine")
    local lazy_status = require("lazy.status")
    local base16_nvim = require("base16-colorscheme")

    base16_nvim.setup()

    -- This line is here because if it's not, the main colorscheme gets
    -- overridden when lualine loads
    vim.cmd("colorscheme onedark")

    lualine.setup({
      options = {
        theme = "onedark",
      },
      sections = {
        lualine_x = {
          {
            lazy_status.updates,
            cond = lazy_status.has_updates,
          },
          { "encoding" },
          { "fileformat" },
          { "filetype" },
        },
      },
    })
  end,
}
