return {
	{
		"Mofiqul/dracula.nvim",
		lazy = false,
		priority = 1000,
		config = function()
			local dracula = require("dracula")

			dracula.setup({
				show_end_of_buffer = false,
				transparent_bg = false,
				italic_comment = true,
				lualine_bg_color = "#44475a",
				overrides = function(colors)
					return {
						CursorLineNr = { fg = colors.orange, bold = true },
						Visual = { bg = colors.visual },

						NormalFloat = { bg = colors.menu, fg = colors.fg },
						FloatBorder = { bg = colors.menu, fg = colors.purple },
						FloatTitle = { bg = colors.menu, fg = colors.pink, bold = true },
					}
				end,
			})

			vim.cmd.colorscheme("dracula")
		end,
	},
}
