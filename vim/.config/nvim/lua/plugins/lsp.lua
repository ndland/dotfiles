return {
	{
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

			vim.lsp.config("zk", {
				default_config = {
					cmd = { "zk", "lsp" },
					filetypes = { "markdown" },
					root_dir = function()
						return vim.loop.cwd()
					end,
					settings = {},
				},
			})

			vim.lsp.enable({ "lua_ls", "ts_ls", "eslint", "zk" })

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
	},
	-- {
	-- 	"nvimdev/lspsaga.nvim",
	-- 	config = function()
	-- 		require("lspsaga").setup({})
	-- 	end,
	-- 	dependencies = {
	-- 		"nvim-treesitter/nvim-treesitter", -- optional
	-- 		"nvim-tree/nvim-web-devicons", -- optional
	-- 	},
	-- },
}
