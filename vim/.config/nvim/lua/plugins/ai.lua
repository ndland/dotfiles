return {
	{
		"CopilotC-Nvim/CopilotChat.nvim",
		dependencies = {
			{ "nvim-lua/plenary.nvim", branch = "master" },
		},
		build = "make tiktoken",
		opts = {},
		keys = {
			{ "<leader>zc", ":CopilotChat<CR>", mode = "n", desc = "Chat with Copilot" },
			{ "<leader>zc", ":CopilotChat<CR>", mode = "v", desc = "Chat with Copilot" },
			{ "<leader>ze", ":CopilotChatExplain<CR>", mode = "v", desc = "Explain code" },
			{ "<leader>zr", ":CopilotChatReview<CR>", mode = "v", desc = "Review code" },
			{ "<leader>zf", ":CopilotChatFix<CR>", mode = "v", desc = "Fix code issues" },
			{ "<leader>zo", ":CopilotChatOptimize<CR>", mode = "v", desc = "Opitmize code" },
			{ "<leader>zd", ":CopilotChatDocs<CR>", mode = "v", desc = "Generate Docs" },
			{ "<leader>zt", ":CopilotChatTests<CR>", mode = "v", desc = "Generate tests" },
			{ "<leader>zm", ":CopilotChatCommit<CR>", mode = "n", desc = "Generate commit message" },
			{ "<leader>zs", ":CopilotChatCommit<CR>", mode = "v", desc = "Generate commit for selection" },
			{ "<leader>zn", ":CopilotChatRename<CR>", mode = "v", desc = "Rename the variable" },
		},
	},
}
