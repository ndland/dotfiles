local status_ok, git_signs = pcall(require, "gitsigns")
if not status_ok then
	return
end

git_signs.setup()
