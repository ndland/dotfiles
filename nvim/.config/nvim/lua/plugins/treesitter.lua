local parsers = {
"astro",
"bash",
"css",
"html",
"javascript",
"json",
"lua",
"markdown",
"markdown_inline",
"tsx",
"typescript",
"vim",
"vimdoc",
}

local filetypes = {
"astro",
"css",
"html",
"javascript",
"javascriptreact",
"json",
"lua",
"markdown",
"sh",
"typescript",
"typescriptreact",
"vim",
"vimdoc",
}

return {
{
"nvim-treesitter/nvim-treesitter",
lazy = false,
build = ":TSUpdate",
config = function()
local treesitter = require("nvim-treesitter")

treesitter.setup()

local installed = {}

for _, language in ipairs(treesitter.get_installed("parsers")) do
installed[language] = true
end

local missing = {}

for _, language in ipairs(parsers) do
if not installed[language] then
table.insert(missing, language)
end
end

if #missing > 0 then
local ok, result = pcall(function()
return treesitter.install(
missing,
{ summary = true }
):wait(300000)
end)

if not ok or not result then
vim.notify(
"nvim-treesitter: failed to install one or more required parsers",
vim.log.levels.ERROR
)
end
end

local group = vim.api.nvim_create_augroup(
"ndland-treesitter",
{ clear = true }
)

vim.api.nvim_create_autocmd("FileType", {
group = group,
pattern = filetypes,
callback = function(args)
local ok = pcall(
vim.treesitter.start,
args.buf
)

if ok then
vim.bo[args.buf].indentexpr =
"v:lua.require'nvim-treesitter'.indentexpr()"
end
end,
})
end,
},
}
