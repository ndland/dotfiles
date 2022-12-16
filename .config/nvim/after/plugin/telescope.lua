local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
	return
end

local builtin = require('telescope.builtin')

telescope.load_extension('projects')
telescope.load_extension('file_browser')

telescope.setup {
	defaults = {
		prompt_prefix = " ",
		selection_caret = " ",
		path_display = { shorten = { len = 2 } },
		vimgrep_arguments = {
			"rg",
			"--hidden",
			"--color=never",
			"--no-heading",
			"--with-filename",
			"--line-number",
			"--column",
			"--smart-case"
		},
	}
}

vim.keymap.set('n', '<leader>gf', builtin.git_files, {})
vim.keymap.set('n', '<leader>b', builtin.buffers, {})
vim.keymap.set('n', '<leader>fb', "<cmd>Telescope file_browser<cr>", {})
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>p', ":lua require'telescope'.extensions.projects.projects{}<cr>", {})
vim.keymap.set('n', '<leader>ss', function()
	builtin.grep_string({ search = vim.fn.input("Grep > ") });
end)
