return {
  "mbbill/undotree",
  config = function()
    local wk = require("which-key")

    wk.add({
      { "<leader>u", group = "undotree" },
      { "<leader>ut", "<cmd>UndotreeToggle<cr>", desc = "Undotree Toggle" },
    })
  end,
}
