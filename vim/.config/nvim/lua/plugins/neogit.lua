return {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim", -- required
		"sindrets/diffview.nvim", -- optional - Diff integration
		"folke/snacks.nvim", -- optional
	},
	config = function()
		local neogit = require("neogit")

		neogit.setup({
			graph_style = "unicode",
		})
	end,
	keys = {
		{
			"<leader>g",
			function()
				require("which-key").show()
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
