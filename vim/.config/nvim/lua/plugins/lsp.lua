return {
	"neovim/nvim-lspconfig",
	event = { "BufReadPre", "BufNewFile", "VeryLazy" },
	keys = {
		{
			"<leader>ca",
			function()
				vim.lsp.buf.code_action()
			end,
			desc = "Code Action",
		},
	},
	config = function()
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

		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("UserLspConfig", {}),
			callback = function(ev)
				local opts = { buffer = ev.buf, silent = true }
			end,
		})

		local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }
		for type, icon in pairs(signs) do
			local hl = "DiagnosticSign" .. type
			vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
		end
	end,
}
