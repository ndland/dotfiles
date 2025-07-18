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
      {
        "<leader><space>",
        function()
          Snacks.picker.smart()
        end,
        desc = "Smart Find Files",
      },
      {
        "<leader>,",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Buffers",
      },
      {
        "<leader>/",
        function()
          Snacks.picker.grep({ hidden = true })
        end,
        desc = "Grep",
      },
      {
        "<leader>:",
        function()
          Snacks.picker.command_history()
        end,
        desc = "Command History",
      },
      { "<leader>e", group = "explorer" },
      {
        "<leader>ef",
        function()
          Snacks.explorer()
        end,
        desc = "File Explorer",
      },
      {
        "<leader>er",
        function()
          Snacks.explorer.reveal()
        end,
        desc = "File Explorer Reveal",
      },
      { "<leader>;", ":nohl<CR>", desc = "Clear search highlights" },
      {
        "gd",
        function()
          Snacks.picker.lsp_definitions()
        end,
        desc = "Goto Definition",
      },
      {
        "gD",
        function()
          Snacks.picker.lsp_declarations()
        end,
        desc = "Goto Declaration",
      },
      {
        "gr",
        function()
          Snacks.picker.lsp_references()
        end,
        nowait = true,
        desc = "References",
      },
      {
        "gI",
        function()
          Snacks.picker.lsp_implementations()
        end,
        desc = "Goto Implementation",
      },
      {
        "gy",
        function()
          Snacks.picker.lsp_type_definitions()
        end,
        desc = "Goto T[y]pe Definition",
      },
      {
        "gs",
        function()
          Snacks.picker.lsp_symbols()
        end,
        desc = "LSP Symbols",
      },
      {
        "gS",
        function()
          Snacks.picker.lsp_workspace_symbols()
        end,
        desc = "LSP Workspace Symbols",
      },

      { "<leader>b", group = "buffers" },
      {
        "<leader>bw",
        function()
          local highlight_enabled
          if highlight_enabled then
            vim.cmd("match none")
            highlight_enabled = false
          else
            vim.cmd("highlight ExtraWhitespace ctermbg=red guibg=#F38BA8")
            vim.cmd("match ExtraWhitespace /\\s\\+$/")
            highlight_enabled = true
          end
        end,
        desc = "Toggle Highlight Trailing Whitespace",
      },
      {
        "<leader>bc",
        function()
          vim.cmd([[%s/\s\+$//e]])
        end,
        desc = "Clear Whitespace Highlight",
      },
      {
        "<leader>bb",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Buffers",
      },
      {
        "<leader>bs",
        function()
          Snacks.picker.grep_buffers()
        end,
        desc = "Grep Open Buffers",
      },
      { "<leader>bn", "<cmd>bn<cr>", desc = "next buffer" },
      { "<leader>bp", "<cmd>bp<cr>", desc = "previous buffer" },
      { "<leader>bo", "<cmd>only<cr>", desc = "delete other splits" },
      { "<leader>bd", group = "delete buffers" },
      { "<leader>bdd", "<cmd>bd<cr>", desc = "delete buffer" },
      { "<leader>bda", "<cmd>bufdo bd<cr>", desc = "delete all buffers" },

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
      {
        "<leader>ff",
        function()
          Snacks.picker.files()
        end,
        desc = "Find Files",
      },
      {
        "<leader>fg",
        function()
          Snacks.picker.git_files()
        end,
        desc = "Find Git Files",
      },
      {
        "<leader>fr",
        function()
          Snacks.picker.recent()
        end,
        desc = "Recent",
      },
      { "<leader>fs", ":w<CR>", desc = "Save" },

      { "<leader>g", group = "Git" },
      {
        "<leader>gB",
        function()
          Snacks.git.blame_line()
        end,
        desc = "Git blame line",
      },
      {
        "<leader>gb",
        function()
          Snacks.picker.git_branches()
        end,
        desc = "Git Branches",
      },
      {
        "<leader>gg",
        function()
          Snacks.lazygit()
        end,
        desc = "Lazygit",
      },
      {
        "<leader>gl",
        function()
          Snacks.picker.git_log()
        end,
        desc = "Git Log",
      },
      {
        "<leader>gL",
        function()
          Snacks.picker.git_log_line()
        end,
        desc = "Git Log Line",
      },
      {
        "<leader>gs",
        function()
          Snacks.picker.git_status()
        end,
        desc = "Git Status",
      },
      {
        "<leader>gS",
        function()
          Snacks.picker.git_stash()
        end,
        desc = "Git Stash",
      },
      {
        "<leader>gd",
        function()
          Snacks.picker.git_diff()
        end,
        desc = "Git Diff (Hunks)",
      },
      {
        "<leader>gf",
        function()
          Snacks.picker.git_log_file()
        end,
        desc = "Git Log File",
      },
      { "<leader>gn", "<cmd>Gitsigns next_hunk<cr>", desc = "Next hunk" },
      { "<leader>gp", "<cmd>Gitsigns prev_hunk<cr>", desc = "Prev hunk" },

      { "<leader>n", group = "notes" },
      {
        "<leader>nd",
        function()
          require("zk.commands").get("ZkNew")({
            dir = "05-daily",
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
          local handle = io.popen("uname -n")
          local computer_name = handle:read("*a"):gsub("%s+", "")
          handle:close()

          local zk_notes_dir

          if computer_name == "VTMACMKXYVH42WL" then
            zk_notes_dir = "~/code/personal/github.com/ndland/zk-notes/" -- Your directory
          else
            zk_notes_dir = "~/code/github.com/ndland/zk-notes/" -- Default directory
          end

          local handle = io.popen("find " .. zk_notes_dir .. " -type d -not -path '*/.*'")
          if handle then
            local result = handle:read("*a")
            handle:close()

            if result then
              local directories = {}
              for dir in result:gmatch("[^\r\n]+") do
                table.insert(directories, dir)
              end

              vim.ui.input({ prompt = "Enter note title: " }, function(title)
                if title then
                  vim.ui.select(directories, { prompt = "Select directory: " }, function(directory)
                    if directory then
                      require("zk.commands").get("ZkNew")({ title = title, dir = directory })
                    end
                  end)
                end
              end)
            end
          end
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

      { "<leader>o", "<cmd>Octo<cr>", desc = "Octo" },

      { "<leader>p", group = "Projects" },
      {
        "<leader>pp",
        function()
          Snacks.picker.projects()
        end,
        desc = "Projects",
      },

      { "<leader>s", group = "search" },
      {
        "<leader>sb",
        function()
          Snacks.picker.lines()
        end,
        desc = "Buffer Lines",
      },
      {
        "<leader>sw",
        function()
          Snacks.picker.grep_word()
        end,
        desc = "Visual selection or word",
        mode = { "n", "x" },
      },
      -- search
      {
        '<leader>s"',
        function()
          Snacks.picker.registers()
        end,
        desc = "Registers",
      },
      {
        "<leader>s/",
        function()
          Snacks.picker.search_history()
        end,
        desc = "Search History",
      },
      {
        "<leader>sa",
        function()
          Snacks.picker.autocmds()
        end,
        desc = "Autocmds",
      },
      {
        "<leader>sb",
        function()
          Snacks.picker.lines()
        end,
        desc = "Buffer Lines",
      },
      {
        "<leader>sc",
        function()
          Snacks.picker.command_history()
        end,
        desc = "Command History",
      },
      {
        "<leader>sC",
        function()
          Snacks.picker.commands()
        end,
        desc = "Commands",
      },
      {
        "<leader>sd",
        function()
          Snacks.picker.diagnostics()
        end,
        desc = "Diagnostics",
      },
      {
        "<leader>sD",
        function()
          Snacks.picker.diagnostics_buffer()
        end,
        desc = "Buffer Diagnostics",
      },
      {
        "<leader>sh",
        function()
          Snacks.picker.help()
        end,
        desc = "Help Pages",
      },
      {
        "<leader>sH",
        function()
          Snacks.picker.highlights()
        end,
        desc = "Highlights",
      },
      {
        "<leader>si",
        function()
          Snacks.picker.icons()
        end,
        desc = "Icons",
      },
      {
        "<leader>sj",
        function()
          Snacks.picker.jumps()
        end,
        desc = "Jumps",
      },
      {
        "<leader>sk",
        function()
          Snacks.picker.keymaps()
        end,
        desc = "Keymaps",
      },
      {
        "<leader>sl",
        function()
          Snacks.picker.loclist()
        end,
        desc = "Location List",
      },
      {
        "<leader>sm",
        function()
          Snacks.picker.marks()
        end,
        desc = "Marks",
      },
      {
        "<leader>sM",
        function()
          Snacks.picker.man()
        end,
        desc = "Man Pages",
      },
      {
        "<leader>sp",
        function()
          Snacks.picker.lazy()
        end,
        desc = "Search for Plugin Spec",
      },
      {
        "<leader>sq",
        function()
          Snacks.picker.qflist()
        end,
        desc = "Quickfix List",
      },
      {
        "<leader>sR",
        function()
          Snacks.picker.resume()
        end,
        desc = "Resume",
      },
      {
        "<leader>su",
        function()
          Snacks.picker.undo()
        end,
        desc = "Undo History",
      },
      {
        "<leader>st",
        function()
          Snacks.picker.todo_comments()
        end,
        desc = "Todo",
      },
      {
        "<leader>sT",
        function()
          Snacks.picker.todo_comments({ keywords = { "PERF", "HACK", "TODO", "FIX", "NOTE", "WARNING", "FIXME" } })
        end,
        desc = "Todo/Fix/Fixme",
      },
      {
        "<leader>l",
        "<cmd>Glow<cr>",
        desc = "Glow",
      },
      {
        "<leader>L",
        "<cmd>Lazy<cr>",
        desc = "Lazy",
      },
      {
        "<leader>M",
        "<cmd>Mason<cr>",
        desc = "Mason",
      },
    })
  end,
  opts = {
    preset = "modern",
  },
}
