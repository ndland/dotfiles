return {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"sindrets/diffview.nvim",
		"folke/snacks.nvim",
	},
	config = function()
		local wk = require("which-key")
		wk.add({
			{ "<leader>g", group = "Git" },
		})
		local neogit = require("neogit")

		neogit.setup({
			integrations = {
				diffview = true,
			},
			graph_style = "unicode",
		})
	end,
	keys = {
		{
			"<leader>gs",
			function()
				require("neogit").open({ kind = "split" })
			end,
			desc = "Git Status",
		},
	},
}
