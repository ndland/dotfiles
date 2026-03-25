return {
	{
		"stevearc/conform.nvim",
		event = { "BufWritePre" },
		cmd = { "ConformInfo" },
		keys = {
			{
				"<leader>lf",
				function()
					require("conform").format({
						async = true,
						lsp_fallback = true,
					})
				end,
				desc = "Format buffer",
			},
		},
		opts = {
			notify_on_error = true,
			format_on_save = function(bufnr)
				local ignore_filetypes = { "markdown" }
				if vim.tbl_contains(ignore_filetypes, vim.bo[bufnr].filetype) then
					return nil
				end

				return {
					timeout_ms = 1500,
					lsp_fallback = true,
				}
			end,
			formatters_by_ft = {
				javascript = { "eslint_d" },
				javascriptreact = { "eslint_d" },
				typescript = { "eslint_d" },
				typescriptreact = { "eslint_d" },
				astro = { "prettierd", "prettier" },
				css = { "prettierd", "prettier" },
				html = { "prettierd", "prettier" },
				json = { "prettierd", "prettier" },
				lua = { "stylua" },
			},
		},
	},
}
