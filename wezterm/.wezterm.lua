local wezterm = require("wezterm")
local act = wezterm.action

local config = {}

if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- BEGIN HOST-SPECIFIC DEFAULT DOMAIN
--
-- One canonical configuration is shared by Windows and macOS.
--
-- Windows:
--   default directly into the selected WSL distribution.
--
-- macOS/Linux:
--   retain native local-domain behavior.
if wezterm.target_triple:find("windows") then
local distro = os.getenv("WEZTERM_WSL_DISTRO") or "Ubuntu-24.04"
local wanted_domain = "WSL:" .. distro
local found = false

for _, domain in ipairs(wezterm.default_wsl_domains()) do
if domain.name == wanted_domain then
found = true
break
end
end

if found then
config.default_domain = wanted_domain
else
wezterm.log_error(
"Requested WSL default domain is unavailable: " .. wanted_domain
)
end
end
-- END HOST-SPECIFIC DEFAULT DOMAIN

-- Appearance
config.color_scheme = "Dracula (Official)"
config.window_background_opacity = 0.97
config.macos_window_background_blur = 0
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = true
config.window_decorations = "RESIZE"
config.scrollback_lines = 10000

-- Fonts (same cuts as Cursor: Term Mono in the terminal, Etoile in chrome)
config.font = wezterm.font_with_fallback({
	{ family = "IosevkaTerm Nerd Font Mono", weight = "Regular" },
	{ family = "Iosevka Nerd Font Mono", weight = "Regular" },
})
config.font_size = 14.0
config.line_height = 1.05
config.harfbuzz_features = { "calt=1", "liga=1" }
config.allow_square_glyphs_to_overflow_width = "WhenFollowedBySpace"
config.use_cap_height_to_scale_fallback_fonts = true
config.front_end = "WebGpu"

config.window_frame = {
	font = wezterm.font({ family = "Iosevka Etoile", weight = "Regular" }),
	font_size = 13.0,
}

config.window_padding = {
	left = 6,
	right = 6,
	top = 6,
	bottom = 6,
}

config.leader = { key = "a", mods = "CTRL", timeout_milliseconds = 2000 }

config.keys = {
	{
		key = "\\",
		mods = "LEADER",
		action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }),
	},
	{
		key = "-",
		mods = "LEADER",
		action = act.SplitVertical({ domain = "CurrentPaneDomain" }),
	},
	{
		key = "h",
		mods = "LEADER",
		action = act.ActivatePaneDirection("Left"),
	},
	{
		key = "j",
		mods = "LEADER",
		action = act.ActivatePaneDirection("Down"),
	},
	{
		key = "k",
		mods = "LEADER",
		action = act.ActivatePaneDirection("Up"),
	},
	{
		key = "l",
		mods = "LEADER",
		action = act.ActivatePaneDirection("Right"),
	},
	{
		key = "x",
		mods = "LEADER",
		action = act.CloseCurrentPane({ confirm = true }),
	},
	{ key = "c", mods = "LEADER", action = act.SpawnTab("CurrentPaneDomain") },
	{ key = "n", mods = "LEADER", action = act.ActivateTabRelative(1) },
	{ key = "p", mods = "LEADER", action = act.ActivateTabRelative(-1) },
	{
		key = "a",
		mods = "LEADER|CTRL",
		action = act.SendKey({ key = "a", mods = "CTRL" }),
	},
}

config.audible_bell = "Disabled"
config.adjust_window_size_when_changing_font_size = false

-- Session save/restore (tmux-resurrect analog). Do not call resurrect.setup():
-- it always restores on gui-startup, which we do not want.
local resurrect = nil

if wezterm.plugin and wezterm.plugin.require then
	local ok, result = pcall(
		wezterm.plugin.require,
		"https://github.com/StephenGemin/resurrect.wezterm"
	)

	if ok then
		resurrect = result
	else
		wezterm.log_error("resurrect.wezterm failed to load: " .. tostring(result))
	end
end

if resurrect then
	resurrect.state_manager.periodic_save({
		interval_seconds = 900,
		save_workspaces = true,
		save_windows = false,
		save_tabs = false,
	})

	if resurrect.pane_tree.add_safe_restore_processes then
		resurrect.pane_tree.add_safe_restore_processes({ "lazygit" })
	end

	-- Restore into THIS window. Do not call restore_workspace(): it no-ops when
	-- the saved workspace is already live, which is the usual case after launch.
	local restore_into_current_window = wezterm.action_callback(function(win, pane)
		resurrect.fuzzy_loader.fuzzy_load(win, pane, function(id)
			local state_type = id:match("^([^/\\]+)")
			local name = id:match("[/\\](.+)$")

			if not name then
				return
			end

			name = name:gsub("%.json$", "")

			local mux_win = win:mux_window()
			local opts = {
				relative = true,
				restore_text = false,
				resize_window = false,
				close_open_tabs = true,
				close_open_panes = true,
				window = mux_win,
				tab = mux_win:active_tab(),
				pane = pane,
				on_pane_restore = resurrect.pane_tree.default_on_pane_restore,
			}

			if state_type == "workspace" then
				local state = resurrect.state_manager.load_state(name, "workspace")
				local window_state = state.window_states and state.window_states[1]

				if not window_state then
					return
				end

				resurrect.window_state.restore_window(mux_win, window_state, opts)

				local current = wezterm.mux.get_active_workspace()
				local saved = state.workspace

				if saved and saved ~= "" and saved ~= current then
					wezterm.mux.rename_workspace(current, saved)
				end
			elseif state_type == "window" then
				local state = resurrect.state_manager.load_state(name, "window")

				resurrect.window_state.restore_window(mux_win, state, opts)
			elseif state_type == "tab" then
				local state = resurrect.state_manager.load_state(name, "tab")

				resurrect.tab_state.restore_tab(mux_win:active_tab(), state, opts)
			end
		end, {
			ignore_windows = true,
			ignore_tabs = true,
		})
	end)

	-- Rename the current workspace in place. SwitchToWorkspace would spawn an
	-- empty workspace and make the existing splits look like they vanished.
	local rename_workspace = wezterm.action_callback(function(win, pane)
		win:perform_action(
			act.PromptInputLine({
				description = "Name this workspace",
				action = wezterm.action_callback(function(_, _, name)
					if not name or name == "" then
						return
					end

					wezterm.mux.rename_workspace(wezterm.mux.get_active_workspace(), name)
				end),
			}),
			pane
		)
	end)

	table.insert(config.keys, {
		key = "w",
		mods = "LEADER",
		action = rename_workspace,
	})
	table.insert(config.keys, {
		key = "s",
		mods = "LEADER",
		action = resurrect.workspace_state.save_workspace_action(),
	})
	table.insert(config.keys, {
		key = "r",
		mods = "LEADER",
		action = restore_into_current_window,
	})
	table.insert(config.keys, {
		key = "d",
		mods = "LEADER",
		action = resurrect.fuzzy_loader.delete_action(),
	})
end

return config
