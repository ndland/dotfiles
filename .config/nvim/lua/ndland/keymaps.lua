vim.g.mapleader = ","

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- Allow to select lines visually, and move them up and down using J & K
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "<leader>y", "\"+y")
vim.keymap.set("v", "<leader>y", "\"+y")
vim.keymap.set("n", "<leader>Y", "\"+Y")
vim.keymap.set("n", "<leader>l", function()
	vim.lsp.buf.format()
end)
