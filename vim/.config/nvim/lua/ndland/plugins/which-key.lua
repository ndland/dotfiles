return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 500
  end,
  config = function()
    local wk = require("which-key")
    local lint = require("lint")

    wk.add({

      { "<leader>b", group = "buffers" },
      { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>bn", "<cmd>bn<cr>", desc = "next buffer" },
      { "<leader>bp", "<cmd>bp<cr>", desc = "previous buffer" },
      { "<leader>bd", "<cmd>bd<cr>", desc = "delete buffer" },

      { "<leader>c", group = "code" },
      { "<leader>ca", vim.lsp.buf.code_action, desc = "Code Actions" },
      { "<leader>cc", "<cmd>CompilerOpen<cr>", desc = "Compiler Open" },
      {
        "<leader>cl",
        function()
          lint.try_lint()
        end,
        desc = "Trigger linting for current file",
      },
      { "<leader>cp", "<cmd>CopilotChatToggle<cr>", desc = "Copilot Chat Toggle" },
      { "<leader>cr", "<cmd>CompilerRedo<cr>", desc = "Compiler Redo" },
      { "<leader>ct", "<cmd>CompilerToggleResults<cr>", desc = "Compiler Toggle Results" },

      { "<leader>f", group = "File" },
      { "<leader>fb", "<cmd>Telescope file_browser<cr>", desc = "File Browser" },
      { "<leader>fc", "<cmd>Telescope grep_string<cr>", desc = "Search under cursor" },
      {
        "<leader>ff",
        "<cmd>lua require('telescope.builtin').fd({ cwd = vim.fn.expand('%:p:h') })<cr>",
        desc = "Find File",
      },
      { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Search" },
      { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent Files" },
      { "<leader>fs", ":w<CR>", desc = "Save" },

      { "<leader>g", group = "Git" },
      { "<leader>gs", "<cmd>LazyGit<cr>", desc = "Open LazyGit" },
      { "<leader>gn", "<cmd>Gitsigns next_hunk<cr>", desc = "Next hunk" },
      { "<leader>gp", "<cmd>Gitsigns prev_hunk<cr>", desc = "Prev hunk" },

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
      {
        "<leader>nd",
        function()
          require("zk.commands").get("ZkNew")({
            dir = "journal/daily",
            date = os.date("%Y-%m-%d"),
            title = os.date("%Y-%m-%d"),
          })
        end,
        desc = "ZK Daily Note",
      },
      { "<leader>nf", "<cmd>ZkNotes<cr>", desc = "ZK Notes" },
      {
        "<leader>nn",
        function()
          vim.ui.input({ prompt = "Enter note title: " }, function(input)
            if input then
              require("zk.commands").get("ZkNew")({ title = input })
            end
          end)
        end,
        desc = "ZK New",
      },
      {
        "<leader>ns",
        function()
          vim.ui.input({ prompt = "Search: " }, function(input)
            if input then
              require("zk.commands").get("ZkNotes")({
                sort = { "modified" },
                match = { input },
              })
            end
          end)
        end,
        desc = "Search ZK Notes",
      },

      { "<leader>p", group = "Projects" },
      { "<leader>pt", "<cmd>TodoTelescope<cr>", desc = "Todos" },
      { "<leader>pp", "<cmd>Telescope projects<cr>", desc = "Projects" },

      { "<leader>t", group = "Telescope" },
      { "<leader>tt", "<cmd>Telescope<cr>", desc = "Telescope" },

      { "<leader>s", group = "splits" },
      { "<leader>sv", "<C-w>v", desc = "Split window vertically" },
      { "<leader>sh", "<C-w>s", desc = "Split window horizontally" },
      { "<leader>se", "<C-w>=", desc = "Make splits equal size" },
      { "<leader>sx", "<cmd>close<CR>", desc = "Close current split" },
      { "<leader>sm", "<cmd>MaximizerToggle<CR>", desc = "Close current split" },

      { "<leader>x", group = "Trouble" },
      { "<leader>xw", "<cmd>Trouble diagnostics toggle<CR>", desc = "Open trouble workspace diagnostics" },
      { "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<CR>", desc = "Open trouble document diagnostics" },
      { "<leader>xq", "<cmd>Trouble quickfix toggle<CR>", desc = "Open trouble quickfix list" },
      { "<leader>xl", "<cmd>Trouble loclist toggle<CR>", desc = "Open trouble location list" },
      { "<leader>xt", "<cmd>Trouble todo toggle<CR>", desc = "Open todos in trouble" },
    })
  end,
  opts = {},
}
