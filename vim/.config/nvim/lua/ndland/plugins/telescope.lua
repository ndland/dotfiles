return {
  "nvim-telescope/telescope.nvim",
  branch = "0.1.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    "nvim-tree/nvim-web-devicons",
    "folke/todo-comments.nvim",
  },

  config = function()
    local telescope = require("telescope")
    local actions = require("telescope.actions")

    telescope.setup({
      defaults = {
        path_display = { "smart" },
        mappings = {
          i = {
            ["<C-k>"] = actions.move_selection_previous, -- move to previous result
            ["<C-j>"] = actions.move_selection_next, -- move to next result
            ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
          },
        },
        vimgrep_arguments = {
          "rg",
          "--color=never",
          "--no-heading",
          "--with-filename",
          "--line-number",
          "--column",
          "--smart-case",
          "--hidden", -- This makes `live_grep` include hidden files
          "--glob",
          "!.git/", -- Excludes .git directory
        },
      },
      pickers = {
        find_files = {
          hidden = true, -- Makes find_files include hidden files
        },
      },
      extensions = {
        media_files = {
          -- filetypes whitelist
          -- defaults to {"png", "jpg", "mp4", "webm", "pdf"}
          filetypes = { "png", "jpg", "mp4", "webm", "pdf", "mkv" },

          find_cmd = "rg", -- find command (defaults to `fd`)
        },
      },
    })

    telescope.load_extension("fzf")

    local wk = require("which-key")
    wk.add({
      { "<leader>t", group = "telescope" },
      { "<leader>tf", "<cmd>Telescope find_files<cr>", desc = "Find File" },
      { "<leader>tr", "<cmd>Telescope oldfiles<cr>", desc = "Recent Files" },
      { "<leader>ts", "<cmd>Telescope live_grep<cr>", desc = "Search" },
      { "<leader>tc", "<cmd>Telescope grep_string<cr>", desc = "Search under cursor" },
      { "<leader>tt", "<cmd>TodoTelescope<cr>", desc = "Todos" },
      { "<leader>tm", "<cmd>Telescope media_files<cr>", desc = "Media Files" },
      { "<leader>tb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>tp", "<cmd>Telescope projects<cr>", desc = "Projects" },
    })
  end,
}
