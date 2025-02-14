-- Treat telekasten files as markdown
vim.api.nvim_create_autocmd("FileType", {
  pattern = "telekasten",
  callback = function()
    vim.bo.filetype = "markdown"
  end,
})

-- Markdown-specific settings
vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function()
    -- Enable soft wrapping
    vim.opt_local.wrap = true
    vim.opt_local.linebreak = true -- Break soft-wrapped lines at word boundaries

    -- Configure hard wrapping for new text
    vim.opt_local.textwidth = 80 -- Set maximum width for new lines
    vim.opt_local.formatoptions:append("t") -- Auto-wrap text as you type
    vim.opt_local.formatoptions:remove("c") -- Avoid comment wrapping for markdown

    -- Additional settings
    vim.opt_local.spell = true -- Enable spell checking
    vim.opt_local.conceallevel = 2 -- Enable conceal for syntax

    -- Indentation settings for Markdown
    vim.opt_local.shiftwidth = 2
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.expandtab = true -- Use spaces instead of tabs for indentation

    -- Ensure proper indentation for lists
    vim.opt_local.formatoptions:append("n") -- Automatically indent new list items
  end,
})

-- Reformat existing lines manually or on save
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.md",
  callback = function()
    -- Uncomment the line below if you want automatic reformatting on save
    -- vim.cmd("normal ggVGgq") -- Hard-wrap the entire buffer to 80 characters
  end,
})
