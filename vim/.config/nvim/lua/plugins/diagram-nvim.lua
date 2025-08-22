return {
	"3rd/diagram.nvim",
	dependencies = {
		{ "3rd/image.nvim", opts = {} }, -- you'd probably want to configure image.nvim manually instead of doing this
	},
	keys = {
		{
			"K", -- or any key you prefer
			function()
				require("diagram").show_diagram_hover()
			end,
			mode = "n",
			ft = { "markdown", "norg" }, -- Only in these filetypes
			desc = "Show diagram in new tab",
		},
	},
	opts = { -- you can just pass {}, defaults below
		events = {
			render_buffer = {},
			clear_buffer = { "BufLeave" },
		},
		renderer_options = {
			mermaid = {
				background = "transparent", -- nil | "transparent" | "white" | "#hex"
				theme = "dark", -- nil | "default" | "dark" | "forest" | "neutral"
				scale = 1, -- nil | 1 (default) | 2  | 3 | ...
				width = nil, -- nil | 800 | 400 | ...
				height = nil, -- nil | 600 | 300 | ...
			},
		},
	},
	config = function()
		require("diagram").setup({
			integrations = {
				require("diagram.integrations.markdown"),
			},
		})
	end,
}
