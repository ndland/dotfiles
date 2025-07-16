-- Auto Commands for Markdown
vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function()
    -- Enable word wrap and better line breaking
    vim.opt_local.wrap = true
    vim.opt_local.linebreak = true -- Break lines at sensible points
    vim.opt_local.breakindent = true -- Keep indentation for wrapped lines
    vim.opt_local.textwidth = 80
    vim.opt_local.showbreak = "↪ " -- Indicate wrapped lines with an arrow

    -- Conceal certain elements (for prettification)
    vim.opt_local.conceallevel = 2
    vim.opt_local.concealcursor = "nc"

    -- Enable spell check
    vim.opt_local.spell = true

    -- Set tab width for readability
    vim.opt_local.tabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true

    -- Enable cursor line for focus
    vim.opt_local.cursorline = true

    -- Syntax Highlighting & Indentation
    -- vim.g.markdown_folding = 1 -- Enable Markdown folding
    vim.g.markdown_syntax_conceal = 0 -- Show syntax normally
    -- vim.g.vim_markdown_folding_disabled = 1 -- No automatic folding
    vim.g.vim_markdown_auto_insert_bullets = 0 -- No auto bullet insertion
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
