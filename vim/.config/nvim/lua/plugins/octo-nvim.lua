return {
	"pwntester/octo.nvim",
	requires = {
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope.nvim",
		-- OR 'ibhagwan/fzf-lua',
		-- OR 'folke/snacks.nvim',
		"nvim-tree/nvim-web-devicons",
	},
	dependencies = {
		"nvim-telescope/telescope.nvim",
	},
	keys = {
		{ "<leader>o", "<CMD>Octo actions<CR>", desc = "Octo" },
	},
	config = function()
		require("octo").setup({
			submit_win = {
				approve_review = { lhs = "<leader>a", desc = "approve review", mode = { "n", "i" } },
				comment_review = { lhs = "<leader>m", desc = "comment review", mode = { "n", "i" } },
				request_changes = { lhs = "<leader>r", desc = "request changes review", mode = { "n", "i" } },
				close_review_tab = { lhs = "<leader>c", desc = "close review tab", mode = { "n", "i" } },
			},
		})
	end,
}
