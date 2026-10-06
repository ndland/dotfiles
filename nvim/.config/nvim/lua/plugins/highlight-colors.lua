return {
	{
		"brenoprata10/nvim-highlight-colors",
		event = { "BufReadPost", "BufNewFile" },
		opts = {
			-- Background rendering makes long Tailwind class strings unreadable,
			-- so use a VS Code style swatch beside the token instead.
			render = "virtual",
			virtual_symbol = "●",
			virtual_symbol_position = "inline",
			virtual_symbol_prefix = "",
			virtual_symbol_suffix = " ",
			enable_tailwind = true,
			exclude_filetypes = { "lazy", "mason", "neo-tree", "snacks_notif" },
			exclude_buftypes = { "terminal", "nofile" },
		},
	},
}
