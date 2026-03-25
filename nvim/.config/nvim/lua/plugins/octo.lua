return {
	{
		"pwntester/octo.nvim",
		cmd = "Octo",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
			"nvim-tree/nvim-web-devicons",
		},
		keys = {
			{ "<leader>gho", "<cmd>Octo<cr>", desc = "GitHub commands" },
			{ "<leader>ghp", "<cmd>Octo pr list<cr>", desc = "List pull requests" },
			{ "<leader>ghi", "<cmd>Octo issue list<cr>", desc = "List issues" },
			{ "<leader>ghn", "<cmd>Octo notification list<cr>", desc = "Notifications" },
			{
				"<leader>ghs",
				function()
					require("octo.utils").create_base_search_command({
						include_current_repo = true,
					})
				end,
				desc = "Search GitHub",
			},
			{ "<leader>ghr", "<cmd>Octo review<cr>", desc = "Review current PR" },
		},
		opts = {
			picker = "telescope",
			enable_builtin = true,
		},
		ssh_aliases = {
			workgit = "github.com",
			personalGit = "github.com",
		},
	},
}
