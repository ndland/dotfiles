return {
	"folke/snacks.nvim",
	priority = 1000,
	enabled = true,
	lazy = false,
	---@type snacks.Config
	opts = {
		dashboard = { example = "github" },
		explorer = { enabled = true },
		input = { enabled = true },
		notifier = {
			enabled = true,
			timeout = 3000,
		},
		picker = {
			enabled = true,
			sources = { files = { hidden = true }, grep = { hidden = true }, explorer = { hidden = true } },
		},
	},
	keys = {
		{
			"<leader>f",
			function()
				Snacks.picker.files()
			end,
			desc = "Files",
		},
		{
			"<leader>,",
			function()
				Snacks.picker.buffers()
			end,
			desc = "Buffers",
		},
		{
			"<leader>/",
			function()
				Snacks.picker.grep()
			end,
			desc = "Grep",
		},
		{
			"<leader>:",
			function()
				Snacks.picker.command_history()
			end,
			desc = "Command History",
		},
		{
			"<leader>e",
			function()
				Snacks.explorer()
			end,
			desc = "File Explorer",
		},
		{
			"<leader>gl",
			function()
				Snacks.picker.git_log()
			end,
			desc = "Git Log",
		},
		{
			"<leader>h",
			function()
				Snacks.picker.help()
			end,
			desc = "Help",
		},
		{ "<leader>s", "<Nop>", desc = "Search" },
		{
			"<leader>sb",
			function()
				Snacks.picker.lines()
			end,
			desc = "Search Buffer",
		},
	},
	init = function()
		vim.api.nvim_create_autocmd("User", {
			pattern = "VeryLazy",
			callback = function()
				-- Create some toggle mappings
				Snacks.toggle.option("spell", { name = "Spelling" }):map("<leader>us")
				Snacks.toggle.option("wrap", { name = "Wrap" }):map("<leader>uw")
				Snacks.toggle.option("relativenumber", { name = "Relative Number" }):map("<leader>uL")
				Snacks.toggle
					.option("conceallevel", { off = 0, on = vim.o.conceallevel > 0 and vim.o.conceallevel or 2 })
					:map("<leader>uc")
				Snacks.toggle
					.option("background", { off = "light", on = "dark", name = "Dark Background" })
					:map("<leader>ub")
				Snacks.toggle.dim():map("<leader>uD")
			end,
		})
	end,
}
