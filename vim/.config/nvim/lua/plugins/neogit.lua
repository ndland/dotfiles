return {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim", -- required
		"sindrets/diffview.nvim", -- optional - Diff integration
		"folke/snacks.nvim", -- optional
	},
	keys = {
		{
			"<leader>g",
			function()
				require("which-key").show({ global = false })
			end,
			desc = "Git",
		},
		{
			"<leader>gs",
			function()
				require("neogit").open({ kind = "split" })
			end,
			desc = "Git Status",
		},
	},
}
