return {
	"folke/trouble.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons", "folke/todo-comments.nvim" },
	opts = {
		focus = true,
	},
	cmd = "Trouble",
}

-- return {
-- 	"folke/trouble.nvim",
-- 	dependencies = {
-- 		"nvim-tree/nvim-web-devicons",
-- 	},
-- 	opts = {
-- 		{
-- 			icons = {
-- 				indent = {
-- 					middle = " ",
-- 					last = " ",
-- 					top = " ",
-- 					ws = "│  ",
-- 				},
-- 			},
-- 			modes = {
-- 				diagnostics = {
-- 					groups = {
-- 						{ "filename", format = "{file_icon} {basename:Title} {count}" },
-- 					},
-- 				},
-- 			},
-- 		},
-- 	}, -- for default options, refer to the configuration section for custom setup.
-- 	cmd = "Trouble",
-- 	keys = {
-- 		{
-- 			"<leader>xx",
-- 			"<cmd>Trouble diagnostics toggle<cr>",
-- 			desc = "Diagnostics (Trouble)",
-- 		},
-- 		{
-- 			"<leader>xX",
-- 			"<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
-- 			desc = "Buffer Diagnostics (Trouble)",
-- 		},
-- 		{
-- 			"<leader>cs",
-- 			"<cmd>Trouble symbols toggle focus=false<cr>",
-- 			desc = "Symbols (Trouble)",
-- 		},
-- 		{
-- 			"<leader>cl",
-- 			"<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
-- 			desc = "LSP Definitions / references / ... (Trouble)",
-- 		},
-- 		{
-- 			"<leader>xL",
-- 			"<cmd>Trouble loclist toggle<cr>",
-- 			desc = "Location List (Trouble)",
-- 		},
-- 		{
-- 			"<leader>xQ",
-- 			"<cmd>Trouble qflist toggle<cr>",
-- 			desc = "Quickfix List (Trouble)",
-- 		},
-- 	},
-- }
