return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    cmd = "Neotree",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      { "<leader>ee", "<cmd>Neotree toggle filesystem left<cr>", desc = "Toggle explorer" },
      { "<leader>ef", "<cmd>Neotree focus filesystem left<cr>", desc = "Focus explorer" },
      { "<leader>er", "<cmd>Neotree reveal filesystem left<cr>", desc = "Reveal current file" },
      { "<leader>eb", "<cmd>Neotree toggle buffers left<cr>", desc = "Buffer explorer" },
      { "<leader>eg", "<cmd>Neotree toggle git_status left<cr>", desc = "Git explorer" },
    },
    opts = {
      close_if_last_window = true,
      popup_border_style = "rounded",
      enable_git_status = true,
      enable_diagnostics = true,

      sources = {
        "filesystem",
        "buffers",
        "git_status",
      },

      source_selector = {
        winbar = true,
        statusline = false,
      },

      default_component_configs = {
        indent = {
          indent_size = 2,
          padding = 1,
          with_markers = true,
          indent_marker = "│",
          last_indent_marker = "└",
          expander_collapsed = "",
          expander_expanded = "",
        },
        icon = {
          folder_closed = "",
          folder_open = "",
          folder_empty = "",
          default = "*",
        },
        git_status = {
          symbols = {
            added = "A",
            deleted = "D",
            modified = "M",
            renamed = "R",
            untracked = "U",
            ignored = "",
            unstaged = "*",
            staged = "+",
            conflict = "!",
          },
        },
      },

      window = {
        position = "left",
        width = 32,
        mappings = {
          ["<space>"] = "none",
        },
      },

      filesystem = {
        follow_current_file = {
          enabled = true,
        },
        hijack_netrw_behavior = "open_default",
        use_libuv_file_watcher = true,
        filtered_items = {
          hide_dotfiles = false,
          hide_gitignored = false,
          never_show = {
            ".DS_Store",
            "thumbs.db",
          },
        },
        window = {
          mappings = {
            ["<cr>"] = "open",
            ["o"] = "open",
            ["l"] = "open",
            ["h"] = "close_node",
            ["<bs>"] = "navigate_up",
            ["."] = "set_root",
            ["H"] = "toggle_hidden",
            ["/"] = "fuzzy_finder",
            ["f"] = "filter_on_submit",
            ["<c-x>"] = "clear_filter",
            ["a"] = "add",
            ["d"] = "delete",
            ["r"] = "rename",
            ["y"] = "copy_to_clipboard",
            ["x"] = "cut_to_clipboard",
            ["p"] = "paste_from_clipboard",
            ["R"] = "refresh",
            ["q"] = "close_window",
          },
        },
      },

      buffers = {
        follow_current_file = {
          enabled = true,
        },
        window = {
          mappings = {
            ["d"] = "buffer_delete",
          },
        },
      },

      git_status = {
        window = {
          mappings = {
            ["A"] = "git_add_all",
            ["gu"] = "git_unstage_file",
            ["ga"] = "git_add_file",
            ["gr"] = "git_revert_file",
            ["gc"] = "git_commit",
            ["gp"] = "git_push",
          },
        },
      },
    },
  },
}

