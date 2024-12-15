return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 500
  end,
  config = function()
    local wk = require("which-key")
    wk.add({
      { "jj", "<ESC>", desc = "Exit insert mode with jj", mode = "i" }, -- group
      { "<leader>b", group = "buffers" },
      { "<leader>bn", "<cmd>bn<cr>", desc = "next buffer" },
      { "<leader>bp", "<cmd>bp<cr>", desc = "previous buffer" },
      { "<leader>bd", "<cmd>bd<cr>", desc = "delete buffer" },

      -- { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File", mode = "n" },
      -- { "<leader>fb", function() print("hello") end, desc = "Foobar" },
      -- { "<leader>fn", desc = "New File" },
      -- { "<leader>f1", hidden = true }, -- hide this keymap
      -- { "<leader>w", proxy = "<c-w>", group = "windows" }, -- proxy to window mappings
    })
  end,
  opts = {},
}

-- keymap.set("n", "<leader>nh", ":nohl<CR>", { desc = "Clear search highlights" })

-- keymap.set("n", "<leader>sv", "<C-w>v", { desc = "Split window vertically" })
-- keymap.set("n", "<leader>sh", "<C-w>s", { desc = "Split window horizontally" })
-- keymap.set("n", "<leader>se", "<C-w>=", { desc = "Make splits equal size" })
-- keymap.set("n", "<leader>sx", "<cmd>close<CR>", { desc = "Close current split" })

-- keymap.set("n", "<leader>to", "<cmd>tabnew<CR>", { desc = "Open new tab" })
-- keymap.set("n", "<leader>tx", "<cmd>tabclose<CR>", { desc = "Close current tab" })
-- keymap.set("n", "<leader>tn", "<cmd>tabn<CR>", { desc = "Go to next tab" })
-- keymap.set("n", "<leader>tp", "<cmd>tabp<CR>", { desc = "Go to previous tab" })
-- keymap.set("n", "<leader>tf", "<cmd>tabnew %<CR>", { desc = "Open current buffer in new tab" })
