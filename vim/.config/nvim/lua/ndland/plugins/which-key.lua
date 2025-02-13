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
          Snacks.picker.grep()
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
      {
        "<leader>e",
        function()
          Snacks.explorer()
        end,
        desc = "File Explorer",
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
      {
        "<leader>fc",
        function()
          Snacks.picker.files({ cwd = vim.fn.stdpath("config") })
        end,
        desc = "Find Config File",
      },
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
          local zk_notes_dir = "~/code/github.com/ndland/zk-notes/" -- Update this to your zk notes directory
          local handle = io.popen("find " .. zk_notes_dir .. " -type d")
          local result = handle:read("*a")
          handle:close()

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
        "<leader>sg",
        function()
          Snacks.picker.grep()
        end,
        desc = "Grep",
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
    })
  end,
  opts = {},
}
