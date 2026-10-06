local map = vim.keymap.set

local select_to_visual = {
	s = "v",
	S = "V",
	["\19"] = "\22",
}

local function tmux_session()
	if not vim.env.TMUX or vim.env.TMUX == "" then
		return nil
	end

	local session = vim.fn.systemlist({ "tmux", "display-message", "-p", "#S" })[1]
	if vim.v.shell_error ~= 0 or not session or session == "" then
		return nil
	end

	return session
end

local function agent_target()
	local session = tmux_session()
	if not session then
		return nil
	end

	return session .. ":agent"
end

local function visual_text()
	local mode = vim.fn.mode()
	mode = select_to_visual[mode] or mode

	if mode ~= "v" and mode ~= "V" and mode ~= "\22" then
		return nil
	end

	local region = vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos("."), { type = mode })
	return table.concat(region, "\n")
end

local function focus_agent(target)
	local out = vim.fn.system({ "tmux", "select-window", "-t", target })
	if vim.v.shell_error ~= 0 then
		vim.notify("Failed to focus " .. target .. ": " .. vim.trim(out), vim.log.levels.ERROR)
		return false
	end

	return true
end

local function send_to_agent(text, focus)
	if text == nil or text == "" then
		vim.notify("Nothing to send to the agent", vim.log.levels.WARN)
		return false
	end

	local target = agent_target()
	if not target then
		vim.notify("No tmux agent window (run nvim from `dev`)", vim.log.levels.ERROR)
		return false
	end

	local out = vim.fn.system({ "tmux", "load-buffer", "-" }, text)
	if vim.v.shell_error ~= 0 then
		vim.notify("tmux load-buffer failed: " .. vim.trim(out), vim.log.levels.ERROR)
		return false
	end

	if focus and not focus_agent(target) then
		return false
	end

	out = vim.fn.system({ "tmux", "paste-buffer", "-d", "-t", target })
	if vim.v.shell_error ~= 0 then
		vim.notify("tmux paste-buffer failed: " .. vim.trim(out), vim.log.levels.ERROR)
		return false
	end

	return true
end

map({ "n", "v" }, "<leader>aa", function()
	local target = agent_target()
	if not target then
		vim.notify("No tmux agent window (run nvim from `dev`)", vim.log.levels.ERROR)
		return
	end

	focus_agent(target)
end, { desc = "Focus agent window" })

map("n", "<leader>ac", function()
	send_to_agent(vim.api.nvim_get_current_line(), true)
end, { desc = "Send line and focus agent" })

map("v", "<leader>ac", function()
	send_to_agent(visual_text(), true)
end, { desc = "Send selection and focus agent" })
