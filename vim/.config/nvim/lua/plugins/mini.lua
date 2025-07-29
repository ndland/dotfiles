return {
	"echasnovski/mini.nvim",
	version = "*",
	keys = {
		{
			"<leader>go",
			function()
				require("mini.diff").toggle_overlay()
			end,
			desc = "Git Overlay",
		},
	},
	config = function()
		-- This is the ONLY way I've found to name a group
		local wk = require("which-key")
		wk.add({
			{ "<leader>s", group = "mini.surround" },
		})
		require("mini.map").setup()
		require("mini.pairs").setup()
		require("mini.diff").setup({
			view = { style = "sign" },
		})
		require("mini.surround").setup({
			mappings = {
				add = "<leader>sa",
				delete = "<leader>sd",
				replace = "<leader>sr",
				find = "<leader>sf",
				find_left = "<leader>sF",
				highlight = "<leader>sh",
				update_n_lines = "<leader>sn",
			},
		})
	end,
}
