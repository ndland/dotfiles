return {
	"neovim/nvim-lspconfig",
	dependencies = { "saghen/blink.cmp" },
	keys = {
		{
			"<leader>d",
			function()
				vim.diagnostic.open_float(nil, { focusable = true })
			end,
			desc = "Open diagnostics in float",
		},
	},
	config = function(_, opts)
		local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }

		vim.lsp.config("lua_ls", {
			settings = {
				Lua = {
					runtime = {
						version = "LuaJIT", -- Neovim uses LuaJIT
					},
					diagnostics = {
						globals = { "vim" },
					},
					workspace = {
						-- Make the server aware of Neovim runtime files
						library = {
							vim.env.VIMRUNTIME,
							unpack(vim.api.nvim_list_runtime_paths()),
						},
						checkThirdParty = false, -- don't prompt for third party
					},
					telemetry = { enable = false },
				},
			},
		})
		vim.lsp.enable({ "lua_ls", "ts_ls", "eslint" })

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
