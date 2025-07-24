return {
	"neovim/nvim-lspconfig",
	dependencies = { "saghen/blink.cmp" },
	keys = {
		{
			"<leader>d",
			function()
				vim.diagnostic.open_float({
					-- You can override global float settings here if needed for this specific command
					focusable = true, -- Make it focusable so you can scroll/copy text
					close_events = { "CursorMoved", "BufLeave", "InsertEnter" }, -- Close when cursor moves or buffer changes
				})
			end,
			desc = "Open diagnostics in float",
		},
	},
	-- example using `opts` for defining servers
	opts = {
		servers = {
			eslint = {},
			lua_ls = {
				settings = {
					Lua = {
						diagnostics = {
							globals = {
								"vim",
								"Snacks",
							},
						},
					},
				},
			},
			ts_ls = {},
		},
	},

	config = function(_, opts)
		local lspconfig = require("lspconfig")
		for server, config in pairs(opts.servers) do
			-- passing config.capabilities to blink.cmp merges with the capabilities in your
			-- `opts[server].capabilities, if you've defined it
			config.capabilities = require("blink.cmp").get_lsp_capabilities(config.capabilities)
			lspconfig[server].setup(config)
		end

		local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }

		vim.diagnostic.config({
			virtual_text = true, -- Shows inline messages
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
