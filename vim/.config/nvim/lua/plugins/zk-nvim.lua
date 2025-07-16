return {
	"zk-org/zk-nvim",
	config = function()
		require("zk").setup({
			-- See Setup section below
			picker = "snacks_picker",
		})
	end,
}
