return {
{
"nvim-neotest/neotest",
dependencies = {
"nvim-neotest/nvim-nio",
"nvim-lua/plenary.nvim",
"marilari88/neotest-vitest",
},
keys = {
{
"<leader>rn",
function()
require("neotest").run.run()
end,
desc = "Run nearest test",
},
{
"<leader>rf",
function()
require("neotest").run.run(vim.fn.expand("%"))
end,
desc = "Run test file",
},
{
"<leader>ra",
function()
require("neotest").run.run(vim.fn.getcwd())
end,
desc = "Run all tests",
},
{
"<leader>rs",
function()
require("neotest").summary.toggle()
end,
desc = "Test summary",
},
{
"<leader>ro",
function()
require("neotest").output.open({
enter = true,
auto_close = true,
})
end,
desc = "Test output",
},
{
"<leader>rO",
function()
require("neotest").output_panel.toggle()
end,
desc = "Test output panel",
},
{
"<leader>rx",
function()
require("neotest").run.stop()
end,
desc = "Stop test",
},
},
config = function()
require("neotest").setup({
adapters = {
require("neotest-vitest")({
filter_dir = function(name)
return not vim.tbl_contains({
".git",
".next",
"build",
"coverage",
"dist",
"node_modules",
"playwright-report",
"test-results",
}, name)
end,
}),
},
})
end,
},
}
