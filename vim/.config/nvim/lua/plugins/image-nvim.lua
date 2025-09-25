return {
	"3rd/image.nvim",
	opts = {
		backend = "wezterm",
		integrations = {
			markdown = {
				enabled = true,
				clear_in_insert_mode = false,
				download_remote_images = true,
				only_render_image_at_cursor = false,
				filetypes = { "markdown", "vimwiki" },
			},
		},
		max_width = 100,
		max_height = 12,
		max_width_window_percentage = nil,
		max_height_window_percentage = 50,
	},
}
