return {
	"zk-org/zk-nvim",
	config = function()
		local opts = { noremap = true, silent = false }

		-- Create a new note after asking for its title.
		vim.api.nvim_set_keymap("n", "<leader>nn", "<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>", opts)

		-- Open notes.
		vim.api.nvim_set_keymap("n", "<leader>no", "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", opts)
		-- Open notes associated with the selected tags.
		vim.api.nvim_set_keymap("n", "<leader>nt", "<Cmd>ZkTags<CR>", opts)

		-- Search for the notes matching a given query.
		vim.api.nvim_set_keymap(
			"n",
			"<leader>nf",
			"<Cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>",
			opts
		)
		-- Search for the notes matching the current visual selection.
		vim.api.nvim_set_keymap("v", "<leader>nf", ":'<,'>ZkMatch<CR>", opts)

		require("zk").setup({
			-- See Setup section below
			picker = "snacks_picker",
		})
	end,
}
