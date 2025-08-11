return {
	"nvim-neotest/neotest",
	dependencies = {
		"thenbe/neotest-playwright",
		"nvim-neotest/nvim-nio",
		"marilari88/neotest-vitest",
	},
	keys = {
		{ "<leader>t", "<Nop>", desc = "test" },
		{
			"<leader>tn",
			function()
				require("neotest").run.run()
			end,
			desc = "Run nearest test",
		},
		{
			"<leader>tt",
			function()
				require("neotest").run.run(vim.fn.expand("%"))
			end,
			desc = "Run file tests",
		},
		{
			"<leader>to",
			function()
				require("neotest").output.open({ enter = true })
			end,
			desc = "Open test output",
		},
	},
	config = function()
		require("neotest").setup({
			adapters = {
				require("neotest-vitest")({
					-- Filter directories when searching for test files. Useful in large projects (see Filter directories notes).
					filter_dir = function(name, rel_path, root)
						return name ~= "node_modules"
					end,
				}),
			},
		})
	end,
}
