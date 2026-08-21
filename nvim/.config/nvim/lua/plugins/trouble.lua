return {
{
"folke/trouble.nvim",
cmd = "Trouble",
opts = {},
keys = {
{
"<leader>xx",
"<cmd>Trouble diagnostics toggle<cr>",
desc = "Diagnostics",
},
{
"<leader>xX",
"<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
desc = "Buffer diagnostics",
},
{
"<leader>xL",
"<cmd>Trouble loclist toggle<cr>",
desc = "Location list",
},
{
"<leader>xQ",
"<cmd>Trouble qflist toggle<cr>",
desc = "Quickfix list",
},
{
"<leader>cs",
"<cmd>Trouble symbols toggle focus=false<cr>",
desc = "Symbols",
},
{
"<leader>cl",
"<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
desc = "LSP definitions / references",
},
},
},
}
