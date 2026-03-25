local wezterm = require("wezterm")

local config = {}

if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- Appearance
config.color_scheme = "Dracula (Official)" -- built-in Dracula[web:585][web:587]
config.window_background_opacity = 0.97
config.macos_window_background_blur = 0 -- ignore if not on macOS
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = true
config.window_decorations = "RESIZE"
config.scrollback_lines = 10000

-- Font
config.font = wezterm.font_with_fallback({
	{ family = "IosevkaTerm NF", weight = "Regular" }, -- main[web:590][web:598]
	{ family = "Iosevka Nerd Font", weight = "Bold" },
	{ family = "JetBrainsMono Nerd Font", weight = "Regular" },
})
config.font_size = 14.0
config.line_height = 1.05

-- Padding
config.window_padding = {
	left = 6,
	right = 6,
	top = 6,
	bottom = 6,
}

-- Leader key (tmux-style)[web:591][web:599]
config.leader = { key = "a", mods = "CTRL", timeout_milliseconds = 2000 }
local act = wezterm.action

config.keys = {
	-- Pane management
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

	-- Tabs
	{ key = "c", mods = "LEADER", action = act.SpawnTab("CurrentPaneDomain") },
	{ key = "n", mods = "LEADER", action = act.ActivateTabRelative(1) },
	{ key = "p", mods = "LEADER", action = act.ActivateTabRelative(-1) },

	-- Send literal Ctrl-A (for tmux, if you use it inside wezterm)[web:591]
	{
		key = "a",
		mods = "LEADER|CTRL",
		action = act.SendKey({ key = "a", mods = "CTRL" }),
	},
}

-- Quality of life
config.audible_bell = "Disabled"
config.adjust_window_size_when_changing_font_size = false

return config
