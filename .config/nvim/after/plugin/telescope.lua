local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
	return
end

local builtin = require('telescope.builtin')

telescope.load_extension('projects')
telescope.load_extension('file_browser')

telescope.setup {
    pickers = {
        find_files = {
            find_command = { 'rg', '--files', '--hidden', '--follow', '--glob', '!.git' },
        },
    },
	defaults = {
		prompt_prefix = " ",
		selection_caret = " ",
		path_display = { shorten = { len = 5 } },
	}
}

vim.keymap.set('n', '<leader>G', builtin.git_files, {})
vim.keymap.set('n', '<leader>b', builtin.buffers, {})
vim.keymap.set('n', '<leader>F', ":Telescope file_browser <cr>", {})
vim.keymap.set('n', '<leader>f', builtin.find_files, {})
vim.keymap.set('n', '<leader>p', telescope.extensions.projects.projects, {})
vim.keymap.set('n', '<leader>s', function() builtin.grep_string({ search = vim.fn.input("Grep > ") }); end)
