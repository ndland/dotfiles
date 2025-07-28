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
	opts = {
		servers = {
			astro = {},
			eslint = {},
			lua_ls = {
				settings = {
					Lua = {
						runtime = {
							version = "LuaJIT",
						},
						diagnostics = {
							globals = {
								"vim",
								"Snacks",
							},
						},
					},
				},
			},
			tailwindcss = {},
			ts_ls = {},
		},
	},

	config = function(_, opts)
		local lspconfig = require("lspconfig")
		for server, config in pairs(opts.servers) do
			config.capabilities = require("blink.cmp").get_lsp_capabilities(config.capabilities)
			lspconfig[server].setup(config)
		end

		local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }

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
