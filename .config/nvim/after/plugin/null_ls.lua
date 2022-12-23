local status_ok, null_ls = pcall(require, "null-ls")
if not status_ok then
	return
end

null_ls.setup({
	debug = true,
	on_attach = null_ls.on_attach,
	sources = {
		null_ls.builtins.diagnostics.eslint,
		null_ls.builtins.formatting.prettier,
		null_ls.builtins.formatting.stylua,
	},
})
