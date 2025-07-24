return {
	"stevearc/conform.nvim",
	opts = {},
	config = function()
		require("conform").setup({
			formatters_by_ft = {
				markdown = { "markdownlint" },
				lua = { "stylua" },
				typescriptreact = { "eslint", "prettier", stop_after_first = true },
			},
			format_on_save = {
				lsp_fallback = true,
				async = false,
				timeout_ms = 1000,
			},
		})
	end,
}
