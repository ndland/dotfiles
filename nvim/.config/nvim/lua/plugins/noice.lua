return {
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		dependencies = {
			"MunifTanjim/nui.nvim",
		},
		keys = {
			{ "<leader>un", "<cmd>NoiceDismiss<cr>", desc = "Dismiss notifications" },
			{ "<leader>uh", "<cmd>Noice history<cr>", desc = "Message history" },
		},
		opts = {
			lsp = {
				-- blink.cmp draws its own signature help; letting noice do it too
				-- stacks two popups on the same keystroke.
				signature = { enabled = false },
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
				},
			},
			presets = {
				command_palette = true,
				long_message_to_split = true,
				lsp_doc_border = true,
			},
		},
	},
}
