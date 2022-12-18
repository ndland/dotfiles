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
		path_display = { shorten = { len = 3 } },
	}
}

vim.keymap.set('n', '<leader>G', builtin.git_files, {})
vim.keymap.set('n', '<leader>b', builtin.buffers, {})
vim.keymap.set('n', '<leader>F', "<cmd>Telescope file_browser<cr>", {})
vim.keymap.set('n', '<leader>f', builtin.find_files, {})
vim.keymap.set('n', '<leader>p', function ()
    telescope.extensions.projects.projects()
end, {})
vim.keymap.set('n', '<leader>s', function()
	builtin.grep_string({ search = vim.fn.input("Grep > ") });
end)
