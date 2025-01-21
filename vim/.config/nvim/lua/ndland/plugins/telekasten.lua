return {
  "renerocksai/telekasten.nvim",
  dependencies = { "nvim-telescope/telescope.nvim" },
  config = function()
    local telekasten = require("telekasten")

    local hostname = vim.fn.hostname()
    local zettle_home

    if hostname == "GMMACANCNXYVH42" then
      zettle_home = vim.fn.expand("~/code/personal/github.com/ndland/zettle/")
    else
      zettle_home = vim.fn.expand("~/code/github.com/ndland/zettle/")
    end

    telekasten.setup({
      home = vim.fn.expand(zettle_home), -- path to zettlekasten
      dailies = vim.fn.expand(zettle_home .. "/daily/"), -- path to daily notes
      weeklies = vim.fn.expand(zettle_home .. "/weekly/"), -- path to weekly notes
      templates = vim.fn.expand(zettle_home .. "/templates/"), -- path to templates
      template_new_daily = vim.fn.expand(zettle_home .. "/templates/daily.md"), -- path to new daily template
      extension = ".md",
      image_subdir = "img",
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "telekasten",
      callback = function()
        vim.bo.filetype = "markdown"
      end,
    })

    vim.api.nvim_create_autocmd("BufEnter", {
      callback = function()
        vim.cmd("setlocal spell")
        vim.opt_local.conceallevel = 2
      end,
    })
  end,
}
