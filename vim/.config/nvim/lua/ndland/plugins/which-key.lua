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
      { "<leader>b", group = "buffers" },
      { "<leader>bn", "<cmd>bn<cr>", desc = "next buffer" },
      { "<leader>bp", "<cmd>bp<cr>", desc = "previous buffer" },
      { "<leader>bd", "<cmd>bd<cr>", desc = "delete buffer" },

      { "<leader>e", group = "editor" },
      { "<leader>eh", ":nohl<CR>", desc = "Clear search highlights" },
      { "<leader>ee", "<cmd>NvimTreeToggle<CR>", desc = "Toggle file explorer" },
      { "<leader>ef", "<cmd>NvimTreeFindFileToggle<CR>", desc = "Toggle file explorer on current file" },
      { "<leader>ec", "<cmd>NvimTreeCollapse<CR>", desc = "Collapse file explorer" },
      { "<leader>er", "<cmd>NvimTreeRefresh<CR>", desc = "Refresh file explorer" },

      { "<leader>et", group = "tabs" },
      { "<leader>eto", "<cmd>tabnew<CR>", desc = "Open new tab" },
      { "<leader>etx", "<cmd>tabclose<CR>", desc = "Close current tab" },
      { "<leader>etn", "<cmd>tabn<CR>", desc = "Go to next tab" },
      { "<leader>etp", "<cmd>tabp<CR>", desc = "Go to previous tab" },
      { "<leader>etf", "<cmd>tabnew %<CR>", desc = "Open current buffer in new tab" },

      { "<leader>n", group = "notes" },
      { "<leader>np", "<cmd>Telekasten panel<cr>", desc = "Telekasten panel" },
      { "<leader>ns", "<cmd>Telekasten search_notes<cr>", desc = "Search Notes" },

      { "<leader>s", group = "splits" },
      { "<leader>sv", "<C-w>v", desc = "Split window vertically" },
      { "<leader>sh", "<C-w>s", desc = "Split window horizontally" },
      { "<leader>se", "<C-w>=", desc = "Make splits equal size" },
      { "<leader>sx", "<cmd>close<CR>", desc = "Close current split" },
      { "<leader>sm", "<cmd>MaximizerToggle<CR>", desc = "Close current split" },
    })
  end,
  opts = {},
}
