return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	opts = {
		preset = "helix",
	},
	keys = {
		{
			"<leader>?",
			function()
				require("which-key").show({ global = false })
			end,
			desc = "Buffer Local Keymaps (which-key)",
		},
		{
			"<leader>b",
			"Nop",
			desc = "Buffers",
		},
		{
			"<leader>bd",
			"<CMD>bd<CR>",
			desc = "Delete Buffer",
		},
	},
}
