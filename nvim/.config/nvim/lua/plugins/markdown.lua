return {
	{
		"MeanderingProgrammer/render-markdown.nvim",
		ft = { "markdown", "quarto", "rmd" },
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"echasnovski/mini.icons",
		},
		opts = {
			render_modes = true,
			sign = { enabled = false },

			heading = {
				sign = false,
				icons = { "󰎤 ", "󰎧 ", "󰎪 ", "󰎭 ", "󰎱 ", "󰎳 " },
			},

			code = {
				sign = false,
				width = "block",
				right_pad = 1,
			},

			bullet = {
				icons = { "●", "○", "◆", "◇" },
			},

			checkbox = {
				enabled = true,
			},

			pipe_table = {
				preset = "round",
			},

			latex = {
				enabled = false,
			},
		},
		config = function(_, opts)
			require("render-markdown").setup(opts)

			vim.keymap.set("n", "<leader>um", function()
				local rm = require("render-markdown")
				rm.set(not rm.get())
			end, { desc = "Toggle Render Markdown" })
		end,
	},
}
