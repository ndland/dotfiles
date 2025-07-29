return {
	"ThePrimeagen/harpoon",
	keys = {
		{ "<leader>h", "<Nop>", desc = "Harpoon" },
		{
			"<leader>ha",
			function()
				require("harpoon.mark").add_file()
			end,
			desc = "Harpoon Add",
		},
		{
			"<leader>ht",
			function()
				require("harpoon.ui").toggle_quick_menu()
			end,
			desc = "Harpoon toggle",
		},
		{
			"<leader>hn",
			function()
				require("harpoon.ui").nav_next()
			end,
			desc = "Harpoon next",
		},
		{
			"<leader>hp",
			function()
				require("harpoon.ui").nav_prev()
			end,
			desc = "Harpoon prev",
		},
	},
}
