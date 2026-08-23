local M = {}

local function notify(message, level)
  vim.notify(message, level or vim.log.levels.INFO, {
    title = "OpenCode",
  })
end

local function run(command)
  local result = vim
    .system(command, {
      text = true,
    })
    :wait()

  return result.code == 0, vim.trim(result.stdout or ""), vim.trim(result.stderr or "")
end

local function copy_to_clipboard(text)
  local copied = pcall(vim.fn.setreg, "+", text)

  if vim.fn.executable("clip.exe") == 1 then
    vim.fn.system({ "clip.exe" }, text)

    if vim.v.shell_error == 0 then
      copied = true
    end
  end

  return copied
end

local function project_path()
  local file = vim.api.nvim_buf_get_name(0)

  if file == "" then
    return nil, "Current buffer has no file path"
  end

  local directory = vim.fs.dirname(file)

  local ok, root = run({
    "git",
    "-C",
    directory,
    "rev-parse",
    "--show-toplevel",
  })

  if not ok or root == "" then
    return vim.fn.fnamemodify(file, ":."), nil
  end

  local relative = vim.fs.relpath(root, file)

  return relative or file, nil
end

local function find_agent()
  if not vim.env.TMUX or vim.env.TMUX == "" then
    return nil, "Not running inside tmux"
  end

  if not vim.env.TMUX_PANE or vim.env.TMUX_PANE == "" then
    return nil, "Current tmux pane is unknown"
  end

  local ok, session = run({
    "tmux",
    "display-message",
    "-p",
    "-t",
    vim.env.TMUX_PANE,
    "#{session_name}",
  })

  if not ok or session == "" then
    return nil, "Unable to determine tmux session"
  end

  local target = session .. ":agent"

  local panes_ok, panes = run({
    "tmux",
    "list-panes",
    "-t",
    target,
    "-F",
    "#{pane_id}\t#{pane_active}\t#{pane_pid}\t#{pane_current_command}",
  })

  if not panes_ok or panes == "" then
    return nil, "This tmux session has no agent window"
  end

  local selected = nil

  for line in panes:gmatch("[^\n]+") do
    local pane, active, pid, command = line:match("([^\t]+)\t([^\t]+)\t([^\t]+)\t(.+)")

    if pane then
      local candidate = {
        session = session,
        window = target,
        pane = pane,
        pid = tonumber(pid),
        command = command or "",
      }

      if active == "1" then
        selected = candidate
        break
      end

      selected = selected or candidate
    end
  end

  if not selected then
    return nil, "Unable to resolve agent pane"
  end

  return selected, nil
end

local function process_tree_contains(root_pid, needle)
  if not root_pid then
    return false
  end

  local ok, output = run({
    "ps",
    "-eww",
    "-o",
    "pid=,ppid=,comm=,args=",
  })

  if not ok or output == "" then
    return false
  end

  local processes = {}
  local children = {}

  for line in output:gmatch("[^\n]+") do
    local pid, ppid, command, arguments = line:match("^%s*(%d+)%s+(%d+)%s+(%S+)%s*(.*)$")

    if pid then
      pid = tonumber(pid)
      ppid = tonumber(ppid)

      processes[pid] = {
        command = command or "",
        arguments = arguments or "",
      }

      children[ppid] = children[ppid] or {}
      table.insert(children[ppid], pid)
    end
  end

  local wanted = needle:lower()
  local queue = { root_pid }
  local cursor = 1
  local seen = {}

  while cursor <= #queue do
    local pid = queue[cursor]
    cursor = cursor + 1

    if not seen[pid] then
      seen[pid] = true

      local process = processes[pid]

      if process then
        local haystack = (process.command .. " " .. process.arguments):lower()

        if haystack:find(wanted, 1, true) then
          return true
        end
      end

      for _, child in ipairs(children[pid] or {}) do
        table.insert(queue, child)
      end
    end
  end

  return false
end

local function agent_is_opencode(agent)
  if agent.command and agent.command:lower():find("opencode", 1, true) then
    return true
  end

  return process_tree_contains(agent.pid, "opencode")
end

local function paste_to_agent(text)
  local agent, err = find_agent()

  if not agent then
    return false, err
  end

  if not agent_is_opencode(agent) then
    return false, "Agent window exists but OpenCode is not running"
  end

  local temporary = vim.fn.tempname()
  local handle, open_error = io.open(temporary, "wb")

  if not handle then
    return false, "Unable to create context buffer: " .. tostring(open_error)
  end

  handle:write(text)
  handle:close()

  local buffer_name = string.format("opencode-context-%d-%d", vim.fn.getpid(), vim.uv.hrtime())

  local loaded, _, load_error = run({
    "tmux",
    "load-buffer",
    "-b",
    buffer_name,
    temporary,
  })

  vim.fn.delete(temporary)

  if not loaded then
    return false, "tmux load-buffer failed: " .. load_error
  end

  local pasted, _, paste_error = run({
    "tmux",
    "paste-buffer",
    "-d",
    "-b",
    buffer_name,
    "-t",
    agent.pane,
  })

  if not pasted then
    run({
      "tmux",
      "delete-buffer",
      "-b",
      buffer_name,
    })

    return false, "tmux paste-buffer failed: " .. paste_error
  end

  local switched, _, switch_error = run({
    "tmux",
    "select-window",
    "-t",
    agent.window,
  })

  if not switched then
    return false, "Context pasted, but window switch failed: " .. switch_error
  end

  return true, nil
end

local function send_context(text, success_message)
  local sent, error_message = paste_to_agent(text)

  if sent then
    notify(success_message)
    return
  end

  if copy_to_clipboard(text) then
    notify(error_message .. "; copied to system clipboard instead", vim.log.levels.WARN)
    return
  end

  notify(error_message .. "; context could not be copied", vim.log.levels.ERROR)
end

local function fence_for(text)
  local longest = 2

  for ticks in text:gmatch("`+") do
    longest = math.max(longest, #ticks)
  end

  return string.rep("`", math.max(3, longest + 1))
end

local function language()
  local aliases = {
    javascriptreact = "jsx",
    typescriptreact = "tsx",
  }

  return aliases[vim.bo.filetype] or vim.bo.filetype
end

function M.jump_to_agent()
  local agent, err = find_agent()

  if not agent then
    notify(err, vim.log.levels.WARN)
    return
  end

  local ok, _, switch_error = run({
    "tmux",
    "select-window",
    "-t",
    agent.window,
  })

  if not ok then
    notify("Unable to switch to agent: " .. switch_error, vim.log.levels.ERROR)
  end
end

function M.add_current_file()
  local relative, err = project_path()

  if not relative then
    notify(err, vim.log.levels.WARN)
    return
  end

  send_context("@" .. relative .. " ", "Added @" .. relative .. " to OpenCode")
end

function M.add_selection()
  local relative, path_error = project_path()

  if not relative then
    notify(path_error, vim.log.levels.WARN)
    return
  end

  local start_position = vim.fn.getpos("v")
  local end_position = vim.fn.getpos(".")
  local visual_type = vim.fn.mode()

  local ok, region = pcall(vim.fn.getregion, start_position, end_position, {
    type = visual_type,
  })

  if not ok or not region or #region == 0 then
    notify("Unable to read visual selection", vim.log.levels.ERROR)
    return
  end

  local text = table.concat(region, "\n")

  local first_line = math.min(start_position[2], end_position[2])

  local last_line = math.max(start_position[2], end_position[2])

  local fence = fence_for(text)

  local context = string.format(
    "Selected context from `%s` (lines %d-%d):\n\n%s%s\n%s\n%s\n\n",
    relative,
    first_line,
    last_line,
    fence,
    language(),
    text,
    fence
  )

  send_context(context, string.format("Added %s:%d-%d to OpenCode", relative, first_line, last_line))
end

return M
