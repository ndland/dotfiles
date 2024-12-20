return {
  "renerocksai/telekasten.nvim",
  dependencies = { "nvim-telescope/telescope.nvim" },
  config = function()
    local telekasten = require("telekasten")

    telekasten.setup({
      home = vim.fn.expand("~/code/github.com/ndland/zettle"),
      dailies = vim.fn.expand("~/code/github.com/ndland/zettle/daily/"), -- path to daily notes
      weeklies = vim.fn.expand("~/code/github.com/ndland/zettle/weekly/"), -- path to weekly notes
      templates = vim.fn.expand("~/code/github.com/ndland/zettle/templates/"), -- path to templates
      template_new_daily = vim.fn.expand("~/code/github.com/ndland/zettle/templates/daily.md"), -- path to new daily template
      extension = ".md",
      image_subdir = "img",
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "telekasten",
      callback = function()
        vim.bo.filetype = "markdown"
      end,
    })
  end,
}
