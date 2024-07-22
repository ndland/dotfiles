local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
  vim.api.nvim_err_writeln("Error loading Telescope")
  return
end

-- Modularize your configuration by requiring separate setup files
-- require('user.telescope') -- Example of a separate file for Telescope setup

local builtin = require('telescope.builtin')
local extensions = require('telescope').extensions.file_browser

-- Load extensions
telescope.load_extension('file_browser')
telescope.load_extension('projects')

-- Telescope setup
telescope.setup {
    -- Configuration for pickers
    pickers = {
        find_files = {
            -- Using ripgrep to include hidden files and follow symlinks
            find_command = { 'rg', '--files', '--hidden', '--follow', '--glob', '!.git' },
        }
    },
    -- Default configuration for Telescope
    defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        path_display = { shorten = { len = 5 } },
    }
}

-- Key mappings
vim.keymap.set('n', '<leader>G', builtin.git_files, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>b', builtin.buffers, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>F', function() extensions.file_browser({ hidden = true }) end, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>f', builtin.find_files, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>s', function() builtin.grep_string({ search = vim.fn.input("Grep > ") }) end, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>p', function() require'telescope'.extensions.projects.projects{} end, { noremap = true, silent = true })

-- Add more comments to explain each part of the configuration

