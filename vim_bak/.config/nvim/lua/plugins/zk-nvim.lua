return {
	"zk-org/zk-nvim",
	keys = {
		{ "<leader>n", "Nop", desc = "notes" },
		{
			"<leader>nb",
			"<Cmd>ZkBacklink<CR>",
			desc = "Backlinks",
		},
		{
			"<leader>nn",
			"<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>",
			desc = "New note with title",
		},
		{
			"<leader>no",
			"<Cmd>ZkNotes { sort = { 'modified' } }<CR>",
			desc = "Open note",
		},
		{
			"<leader>nf",
			"<Cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>",
			desc = "Find notes",
		},
		{
			"<leader>nt",
			"<Cmd>ZkTags<CR>",
			desc = "Find tags",
		},
	},
	config = function()
		require("zk").setup({
			-- See Setup section below
			picker = "snacks_picker",
		})
	end,
}
