return {
	"mason-org/mason-lspconfig.nvim",
	opts = {
		ensure_installed = {
			"emmet_ls",
			"html",
			"lua_ls",
			"marksman",
			"tailwindcss",
			"ts_ls",
			"zk",
		},
	},
	dependencies = {
		{ "mason-org/mason.nvim", opts = {} },
		"neovim/nvim-lspconfig",
		"WhoIsSethDaniel/mason-tool-installer.nvim",
	},
	config = function()
		local mason_tool_installer = require("mason-tool-installer")

		mason_tool_installer.setup({
			ensure_installed = {
				"eslint_d",
				"markdownlint",
				"prettier", -- prettier formatter
				"stylua", -- lua formatter
				"zk",
			},
		})
	end,
}
