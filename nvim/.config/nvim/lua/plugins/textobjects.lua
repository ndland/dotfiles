-- Must track nvim-treesitter's `main` branch. The two `main` rewrites are a
-- matched pair and cannot be mixed with either project's `master`.
return {
	{
		"nvim-treesitter/nvim-treesitter-textobjects",
		branch = "main",
		event = { "BufReadPost", "BufNewFile" },
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
		},
		config = function()
			require("nvim-treesitter-textobjects").setup({
				select = {
					-- Act on the next textobject when the cursor sits outside one,
					-- so `cif` works from a blank line between functions.
					lookahead = true,
				},
			})

			local select = require("nvim-treesitter-textobjects.select")
			local move = require("nvim-treesitter-textobjects.move")

			local selections = {
				af = "@function.outer",
				["if"] = "@function.inner",
				ac = "@class.outer",
				ic = "@class.inner",
				aa = "@parameter.outer",
				ia = "@parameter.inner",
			}

			for lhs, capture in pairs(selections) do
				vim.keymap.set({ "x", "o" }, lhs, function()
					select.select_textobject(capture, "textobjects")
				end, { desc = "Select " .. capture })
			end

			-- `]m`/`[[` rather than `]f`/`]c`: this is the upstream convention,
			-- and `]c` is Vim's own jump-to-next-change in diff mode.
			local movements = {
				["]m"] = { move.goto_next_start, "@function.outer" },
				["]M"] = { move.goto_next_end, "@function.outer" },
				["[m"] = { move.goto_previous_start, "@function.outer" },
				["[M"] = { move.goto_previous_end, "@function.outer" },
				["]]"] = { move.goto_next_start, "@class.outer" },
				["]["] = { move.goto_next_end, "@class.outer" },
				["[["] = { move.goto_previous_start, "@class.outer" },
				["[]"] = { move.goto_previous_end, "@class.outer" },
			}

			for lhs, spec in pairs(movements) do
				local goto_fn, capture = spec[1], spec[2]
				vim.keymap.set({ "n", "x", "o" }, lhs, function()
					goto_fn(capture, "textobjects")
				end, { desc = "Jump " .. lhs .. " (" .. capture .. ")" })
			end
		end,
	},
}
