return {
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			preset = "modern",
			delay = 300,
			notify = false,
		},
		config = function(_, opts)
			local wk = require("which-key")
			wk.setup(opts)

			wk.add({
				{ "<leader>c", group = "Code" },
				{ "<leader>e", group = "Explorer" },
				{ "<leader>f", group = "Find" },
				{ "<leader>g", group = "Git" },
				{ "<leader>gv", group = "Git View" },
				{ "<leader>gd", group = "GitSigns diffthis" },
				{ "<leader>gh", group = "GitHub" },
				{ "<leader>l", group = "LSP" },
				{ "<leader>n", group = "Notes" },
				{ "<leader>t", group = "Terminal" },
				{ "<leader>x", group = "Problems" },
			})
		end,
	},
}
