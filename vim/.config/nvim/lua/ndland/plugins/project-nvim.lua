return {
  "ahmedkhalf/project.nvim",
  config = function()
    require("project_nvim").setup({
      patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json", "Cargo.toml", "go.mod" },
      ignore_lsp = { "efm", "sumneko_lua", "tsserver" },
      silent_chdir = true,
    })
    require("nvim-tree").setup({
      sync_root_with_cwd = true,
      respect_buf_cwd = true,
      update_focused_file = {
        enable = true,
        update_root = true,
      },
    })
    require("telescope").load_extension("projects")
  end,
}
