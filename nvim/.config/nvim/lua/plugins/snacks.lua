return {
	{
		"folke/snacks.nvim",
		priority = 1000,
		lazy = false,
		opts = {
			bigfile = { enabled = true },
			quickfile = { enabled = true },
			rename = { enabled = true },

			input = {
				enabled = true,
				win = {
					relative = "cursor",
					row = -3,
					col = 0,
				},
			},

			notifier = {
				-- noice owns vim.notify and renders through snacks as its backend,
				-- so snacks must not claim vim.notify too. Styling below still applies.
				enabled = false,
				timeout = 3000,
				style = "compact",
			},

			indent = {
				enabled = true,
				indent = { char = "│" },
				scope = { char = "│" },
			},

			scroll = { enabled = true },

			statuscolumn = {
				enabled = true,
				folds = { open = true },
			},
		},
		keys = {
			{
				"<leader>lR",
				function()
					require("snacks").rename.rename_file()
				end,
				desc = "Rename file",
			},
		},
	},
}
