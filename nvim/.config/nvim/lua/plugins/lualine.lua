local function diff_source()
	local gitsigns = vim.b.gitsigns_status_dict
	if gitsigns then
		return {
			added = gitsigns.added,
			modified = gitsigns.changed,
			removed = gitsigns.removed,
		}
	end
end

local function lsp_clients()
	local clients = vim.lsp.get_clients({ bufnr = 0 })
	if #clients == 0 then
		return ""
	end

	local names = vim.tbl_map(function(client)
		return client.name
	end, clients)

	return "󰒋 " .. table.concat(names, ", ")
end

return {
	{
		"nvim-lualine/lualine.nvim",
		event = "VeryLazy",
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		opts = {
			options = {
				theme = "dracula-nvim",
				globalstatus = true,
				component_separators = { left = "", right = "" },
				section_separators = { left = "", right = "" },
				disabled_filetypes = {
					statusline = { "neo-tree" },
				},
			},
			sections = {
				lualine_a = { "mode" },
				lualine_b = {
					{ "branch", icon = "" },
					{
						"diff",
						source = diff_source,
						symbols = { added = " ", modified = " ", removed = " " },
					},
				},
				lualine_c = {
					{
						"filename",
						path = 1,
						symbols = { modified = " ", readonly = " ", unnamed = "[No Name]" },
					},
					{
						"diagnostics",
						sources = { "nvim_diagnostic" },
						symbols = { error = "󰅚 ", warn = "󰀪 ", info = "󰋽 ", hint = "󰌶 " },
					},
				},
				lualine_x = {
					{
						lsp_clients,
						cond = function()
							return vim.o.columns > 100
						end,
					},
					"filetype",
				},
				lualine_y = { "progress" },
				lualine_z = { "location" },
			},
			extensions = { "neo-tree", "toggleterm", "lazy", "mason", "quickfix" },
		},
	},
}
