local M = {}

local last_root
local last_file

local fallback_markers = {
  "pnpm-workspace.yaml",
  "package.json",
  "pyproject.toml",
  "Cargo.toml",
  "go.mod",
  "mise.toml",
}

local function buffer_path(bufnr)
  bufnr = bufnr or 0

  if not vim.api.nvim_buf_is_valid(bufnr) then
    return nil
  end

  if vim.bo[bufnr].buftype ~= "" then
    return nil
  end

  local name = vim.api.nvim_buf_get_name(bufnr)

  if name == "" then
    return nil
  end

  return vim.fs.normalize(name)
end

local function marker_root(start, markers)
  local matches = vim.fs.find(markers, {
    path = start,
    upward = true,
  })

  if #matches == 0 then
    return nil
  end

  return vim.fs.dirname(matches[1])
end

local function root_for_path(path)
  if not path or path == "" then
    return nil
  end

  local stat = vim.uv.fs_stat(path)
  local start

  if stat and stat.type == "directory" then
    start = path
  else
    start = vim.fs.dirname(path)
  end

  if not start or start == "" then
    return nil
  end

  -- Git is authoritative. A nested package.json in a monorepo
  -- must not narrow the editor below the repository root.
  local git_root = marker_root(start, { ".git" })

  if git_root then
    return vim.fs.normalize(git_root)
  end

  local marker = marker_root(start, fallback_markers)

  if marker then
    return vim.fs.normalize(marker)
  end

  return vim.fs.normalize(start)
end

function M.for_buffer(bufnr)
  return root_for_path(buffer_path(bufnr))
end

function M.current_file(bufnr)
  local file = buffer_path(bufnr)

  if file then
    last_file = file
    return file
  end

  return last_file
end

function M.current(bufnr)
  local file = buffer_path(bufnr)

  if file then
    local root = root_for_path(file)

    last_file = file

    if root then
      last_root = root
      return root
    end
  end

  local ok, root = pcall(vim.api.nvim_win_get_var, 0, "project_root")

  if ok and type(root) == "string" and root ~= "" then
    return root
  end

  return last_root or vim.fn.getcwd()
end

function M.sync(bufnr)
  local file = buffer_path(bufnr)

  if not file then
    return M.current(bufnr)
  end

  local root = root_for_path(file)

  if not root then
    return M.current(bufnr)
  end

  last_file = file
  last_root = root

  vim.w.project_root = root

  -- Tab-local by design. Neo-tree sidebars synchronize with
  -- the tab cwd, so the active file's project becomes the sidebar root
  -- without changing Neovim's global cwd.
  vim.cmd.tcd(vim.fn.fnameescape(root))

  return root
end

function M.setup()
  local group = vim.api.nvim_create_augroup("current_file_project_root", { clear = true })

  vim.api.nvim_create_autocmd("BufEnter", {
    group = group,
    callback = function(args)
      M.sync(args.buf)
    end,
  })

  -- Handles direct :Telescope usage in addition to our mappings.
  vim.api.nvim_create_autocmd("User", {
    group = group,
    pattern = "TelescopeFindPre",
    callback = function()
      local root = M.current()

      if root and root ~= "" then
        vim.cmd.tcd(vim.fn.fnameescape(root))
      end
    end,
  })

  M.sync(0)
end

return M
