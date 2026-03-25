local filename = vim.fn.expand("%:t") -- just the filename, no path
if filename == "CHANGELOG.md" then
	return
end

-- Enable soft wrap for readability
vim.opt_local.wrap = true
vim.opt_local.linebreak = true

-- Hard wrap at 80 chars when typing
vim.opt_local.textwidth = 80
vim.opt_local.formatoptions:append("t") -- wrap text when typing
vim.opt_local.formatoptions:remove("l") -- don't leave long lines alone

-- Markdown-specific mappings
vim.keymap.set("n", "<leader>q", "gqap", { buffer = true, desc = "Reflow paragraph" })
