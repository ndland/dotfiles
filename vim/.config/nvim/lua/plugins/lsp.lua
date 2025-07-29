return {
	"neovim/nvim-lspconfig",
	dependencies = { "saghen/blink.cmp" },
	keys = {
		{
			"<leader>d",
			function()
				vim.diagnostic.open_float({
					focusable = true,
					close_events = { "CursorMoved", "BufLeave", "InsertEnter" },
				})
			end,
			desc = "Open diagnostics in float",
		},
	},
	config = function(_, opts)
		local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }

		vim.lsp.enable("ts_ls")
		vim.lsp.config("lua_ls", {
			settings = {
				Lua = {
					runtime = {
						-- Tell the language server which version of Lua you're using
						-- (most likely LuaJIT in the case of Neovim)
						version = "LuaJIT",
					},
					diagnostics = {
						-- Get the language server to recognize the `vim` global
						globals = {
							"vim",
							"require",
						},
					},
				},
			},
		})
		vim.lsp.enable("eslint")

		vim.diagnostic.config({
			virtual_text = true,
			signs = {
				active = true,
				text = {
					[vim.diagnostic.severity.ERROR] = signs.Error,
					[vim.diagnostic.severity.WARN] = signs.Warn,
					[vim.diagnostic.severity.INFO] = signs.Info,
					[vim.diagnostic.severity.HINT] = signs.Hint,
				},
			},
			underline = true,
			update_in_insert = false,
			severity_sort = true,
		})
	end,
}
