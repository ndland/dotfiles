return {
	"echasnovski/mini.nvim",
	version = "*",
	config = function()
		require("mini.map").setup()
		require("mini.pairs").setup()
	end,
}
