return {
	"mfussenegger/nvim-lint",
	-- Lint on these events. BufReadPost covers opening files, BufWritePost covers saving.
	event = { "BufReadPost", "BufWritePost" },
	config = function()
		local lint = require("lint")

		-- Define which linters to use for which filetypes
		lint.linters_by_ft = {
			markdown = { "proselint" },
		}

		-- Create an autocommand group to manage linting callbacks
		vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost" }, {
			group = vim.api.nvim_create_augroup("LintGroup", { clear = true }),
			callback = function()
				lint.try_lint() -- This function runs linters for the current buffer
			end,
		})
	end,
}
