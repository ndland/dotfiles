local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
	return
end

local builtin = require('telescope.builtin')
local extensions = require('telescope').extensions.file_browser

telescope.load_extension('file_browser')

telescope.setup {
    pickers = {
        find_files = {
            find_command = { 'rg', '--files', '--hidden', '--follow', '--glob', '!.git' },
        }
    },
	defaults = {
		prompt_prefix = " ",
		selection_caret = " ",
		path_display = { shorten = { len = 5 } },
	}
}

vim.keymap.set('n', '<leader>G', builtin.git_files, {})
vim.keymap.set('n', '<leader>b', builtin.buffers, {})
vim.keymap.set('n', '<leader>F', function() extensions.file_browser({ hidden = true }) end, {})
vim.keymap.set('n', '<leader>f', builtin.find_files, {})
vim.keymap.set('n', '<leader>1', function() builtin.find_files({ find_command = {'fd', '.', '--max-depth', '1', '--type', 'f', '--hidden'}, }) end, {})
vim.keymap.set('n', '<leader>s', function() builtin.grep_string({ search = vim.fn.input("Grep > ") }); end)
