return {
  "renerocksai/telekasten.nvim",
  dependencies = { "nvim-telescope/telescope.nvim" },
  config = function()
    -- import nvim-autopairs
    local telekasten = require("telekasten")

    -- configure autopairs
    telekasten.setup({
      home = vim.fn.expand("~/code/github.com/ndland/zettle"),
    })

    -- set keymaps
    local keymap = vim.keymap -- for conciseness

    keymap.set("n", "<leader>tp", "<cmd>Telekasten panel<cr>", { desc = "Telekasten panel" })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "telekasten",
      callback = function()
        vim.bo.filetype = "markdown"
      end,
    })
  end,
}
