return {
	{
		"akinsho/toggleterm.nvim",
		version = "*",
		cmd = { "ToggleTerm", "TermExec", "ToggleTermToggleAll", "TermSelect" },
		keys = {
			{ "<leader>tt", "<cmd>ToggleTerm direction=float<cr>", desc = "Toggle terminal" },
			{ "<leader>tf", "<cmd>ToggleTerm direction=float<cr>", desc = "Float terminal" },
			{ "<leader>th", "<cmd>ToggleTerm direction=horizontal size=15<cr>", desc = "Horizontal terminal" },
			{ "<leader>tv", "<cmd>ToggleTerm direction=vertical size=60<cr>", desc = "Vertical terminal" },
			{
				"<leader>tn",
				function()
					local Terminal = require("toggleterm.terminal").Terminal
					local terms = require("toggleterm.terminal").get_all()
					local used = {}

					for _, term in pairs(terms) do
						if term.count then
							used[term.count] = true
						end
					end

					local count = 1
					while used[count] do
						count = count + 1
					end

					local term = Terminal:new({
						count = count,
						direction = "float",
						hidden = false,
						close_on_exit = false,
						float_opts = {
							border = "rounded",
						},
					})

					term:toggle()
				end,
				desc = "New terminal",
			},
			{
				"<leader>ts",
				"<cmd>TermSelect<cr>",
				desc = "Select terminal",
			},
			{
				"<leader>ta",
				"<cmd>ToggleTermToggleAll<cr>",
				desc = "Toggle all terminals",
			},
			{
				"<leader>tg",
				function()
					_G.toggle_lazygit()
				end,
				desc = "Lazygit",
			},
		},
		opts = {
			open_mapping = nil,
			hide_numbers = true,
			shade_filetypes = {},
			start_in_insert = true,
			insert_mappings = true,
			terminal_mappings = true,
			persist_size = true,
			persist_mode = true,
			direction = "float",
			close_on_exit = true,
			float_opts = {
				border = "rounded",
				winblend = 0,
			},
			size = function(term)
				if term.direction == "horizontal" then
					return 15
				elseif term.direction == "vertical" then
					return math.floor(vim.o.columns * 0.40)
				end
			end,
			on_open = function(term)
				vim.cmd("startinsert!")
				local opts = { buffer = term.bufnr, silent = true }
				vim.keymap.set("t", "<Esc><Esc>", [[<C-\><C-n>]], opts)
				vim.keymap.set("t", "<C-h>", [[<Cmd>wincmd h<CR>]], opts)
				vim.keymap.set("t", "<C-j>", [[<Cmd>wincmd j<CR>]], opts)
				vim.keymap.set("t", "<C-k>", [[<Cmd>wincmd k<CR>]], opts)
				vim.keymap.set("t", "<C-l>", [[<Cmd>wincmd l<CR>]], opts)
			end,
		},
		config = function(_, opts)
			require("toggleterm").setup(opts)

			local Terminal = require("toggleterm.terminal").Terminal

			local lazygit = Terminal:new({
				cmd = "lazygit",
				dir = "git_dir",
				direction = "float",
				hidden = true,
				close_on_exit = false,
				float_opts = {
					border = "rounded",
				},
			})

			function _G.toggle_lazygit()
				lazygit:toggle()
			end
		end,
	},
}
