vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
	pattern = "*.mdx",
	callback = function()
		vim.bo.filetype = "markdown"
	end,
})
