return {
	"neovim/nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile", "VeryLazy" },
	opts = {
		diagnostics = { virtual_text = { prefix = "icons" } },
	},
	keys = {
		{
			"<leader>ca",
			function()
				vim.lsp.buf.code_action()
			end,
			desc = "Code Action",
		},
		{
			"<leader>d",
			function()
				-- Call the Neovim built-in function to open the diagnostic float
				vim.diagnostic.open_float(nil, {
					-- Optional: Customize the float window appearance specifically for this action
					focusable = false,
					close_events = { "CursorMoved", "CursorMovedI", "BufHidden", "InsertCharPre", "WinLeave" },
					border = "rounded",
				})
			end,
			desc = "Show Line Diagnostics (Float)", -- This is what which-key will display
		},
	},
	config = function()
		if vim.fn.has("nvim-0.11") == 1 and vim.lsp.config then
			vim.lsp.config("*", {
				capabilities = require("blink.cmp").get_lsp_capabilities(),
			})
		end
		local configs = require("lspconfig/configs")

		configs.zk = {
			default_config = {
				cmd = { "zk", "lsp" },
				filetypes = { "markdown" },
				root_dir = function()
					return vim.loop.cwd()
				end,
				settings = {},
			},
		}

		vim.diagnostic.config({
			signs = {
				text = {
					[vim.diagnostic.severity.ERROR] = "󰅖 ",
					[vim.diagnostic.severity.WARN] = " ",
					[vim.diagnostic.severity.INFO] = " ",
					[vim.diagnostic.severity.HINT] = "󰌵 ",
				},
			},
		})
	end,
}
