return {
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		event = { "BufReadPost", "BufNewFile" },
		main = "nvim-treesitter.configs",
		opts = {
			ensure_installed = {
				"astro",
				"bash",
				"css",
				"html",
				"javascript",
				"json",
				"lua",
				"markdown",
				"markdown_inline",
				"tsx",
				"typescript",
				"vim",
				"vimdoc",
			},
			auto_install = true,
			highlight = {
				enable = true,
			},
			indent = {
				enable = true,
			},
			incremental_selection = {
				enable = false,
				keymaps = {
					init_selection = "<CR>",
					node_incremental = "<CR>",
					scope_incremental = "<S-CR>",
					node_decremental = "<BS>",
				},
			},
		},
	},
}
