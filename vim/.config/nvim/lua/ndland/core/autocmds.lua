vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = { "*.md" }, -- Adjust pattern if your notes use a different extension
  callback = function()
    if vim.bo.filetype == "telekasten" then
      vim.bo.filetype = "markdown"
    end
  end,
})

-- Enable line wrapping for markdown files
vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function()
    vim.opt.wrap = true
    vim.opt.textwidth = 80
    vim.opt.linebreak = true -- Ensure wrapped lines break at convenient points end
  end,
})
