-- NDLAND_CONVENTIONAL_COMMIT_TEMPLATE_START
--
-- Conventional Commit guidance for Neovim gitcommit buffers.
--
-- Important design rule:
--
-- Git/Neogit owns the commit editor lifecycle.
-- Husky/commitlint owns validation.
-- This file only provides editor guidance.
--
-- The template is inserted only when the commit message contains
-- no meaningful non-comment text. Existing amend/reword/merge/
-- revert messages are therefore preserved.
--
-- Approved scopes are read from the current repository's
-- commitlint.config.js so the validator remains the single source
-- of truth.

if vim.b.ndland_conventional_commit_template_applied then
  return
end

vim.b.ndland_conventional_commit_template_applied = true

local function has_meaningful_commit_text(lines)
  for _, line in ipairs(lines) do
    local text = vim.trim(line)

    if text ~= "" and not vim.startswith(text, "#") then
      return true
    end
  end

  return false
end

local function find_git_root()
  local name = vim.api.nvim_buf_get_name(0)

  if name ~= "" then
    local start = vim.fs.dirname(name)
    local root = vim.fs.root(start, ".git")

    if root then
      return root
    end
  end

  return vim.fs.root(vim.fn.getcwd(), ".git")
end

local function read_commitlint_scopes(root)
  local path = root .. "/commitlint.config.js"

  if vim.fn.filereadable(path) ~= 1 then
    return nil
  end

  local lines = vim.fn.readfile(path)
  local text = table.concat(lines, "\n")

  if not text:find('"scope%-enum"%s*:') then
    return nil
  end

  local scopes = {}
  local inside = false

  for _, line in ipairs(lines) do
    if not inside and line:match("^%s*const%s+scopes%s*=%s*%[") then
      inside = true
    elseif inside and line:match("^%s*%];") then
      break
    elseif inside then
      local scope =
        line:match([["([^"]+)"]])
        or line:match([['([^']+)']])

      if scope then
        table.insert(scopes, scope)
      end
    end
  end

  if #scopes == 0 then
    return nil
  end

  return scopes
end

local current_lines =
  vim.api.nvim_buf_get_lines(0, 0, -1, false)

if has_meaningful_commit_text(current_lines) then
  return
end

local root = find_git_root()

if not root then
  return
end

local scopes = read_commitlint_scopes(root)

if not scopes then
  return
end

local template = {
  "",
  "",
  "",
  "# Conventional Commit",
  "#",
  "# Subject - line 1:",
  "#   <type>(<scope>): <subject>",
  "#",
  "# Body - optional:",
  "#   leave line 2 blank and start the body on line 3",
  "#",
  "# Types:",
  "#   feat fix refactor test docs chore build ci perf style revert",
  "#",
  "# Approved scopes (read from commitlint.config.js):",
  "#   " .. table.concat(scopes, "  "),
  "#",
  "# Scope is optional:",
  "#   docs: clarify local setup",
  "#",
  "# Prefer a domain scope when the change is domain behavior:",
  "#   feat(transactions): add date-range filtering",
  "#",
  "# Use application/test scopes for changes to those layers:",
  "#   build(web): configure Vite alias",
  "#   test(e2e): stabilize account navigation",
  "#",
  "# Add a new scope only in commitlint.config.js.",
  "# This guide reads that list automatically on the next commit.",
  "#",
  "# Example with body:",
  "#",
  "#   feat(transactions): add date-range filtering",
  "#",
  "#   Apply the selected range to the transaction list and all",
  "#   range-sensitive summary calculations.",
}

vim.api.nvim_buf_set_lines(
  0,
  0,
  0,
  false,
  template
)

local buf = vim.api.nvim_get_current_buf()

vim.schedule(function()
  if not vim.api.nvim_buf_is_valid(buf) then
    return
  end

  if vim.api.nvim_get_current_buf() ~= buf then
    return
  end

  pcall(
    vim.api.nvim_win_set_cursor,
    0,
    { 1, 0 }
  )
end)

-- NDLAND_CONVENTIONAL_COMMIT_TEMPLATE_END
