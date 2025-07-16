return {
	"neovim/nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile", "VeryLazy" },
	keys = {
		-- Top Pickers & Explorer
		{
			"<leader>ca",
			function()
				vim.lsp.buf.code_action()
			end,
			desc = "Code Action",
		},
	},
	config = function()
		local lspconfig = require("lspconfig")
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

		lspconfig.zk.setup({
			on_attach = function(client, buffer)
				-- Add keybindings here, see https://github.com/neovim/nvim-lspconfig#keybindings-and-completion
			end,
		})

		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("UserLspConfig", {}),
			callback = function(ev)
				-- Buffer local mappings.
				-- See `:help vim.lsp.*` for documentation on any of the below functions
				local opts = { buffer = ev.buf, silent = true }
			end,
		})

		-- Change the Diagnostic symbols in the sign column (gutter)
		-- (not in youtube nvim video)
		local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }
		for type, icon in pairs(signs) do
			local hl = "DiagnosticSign" .. type
			vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
		end
	end,
}
