return {
  {
    "nvim-telescope/telescope.nvim",
    version = "*",
    cmd = "Telescope",
    init = function()
      vim.api.nvim_create_autocmd("VimEnter", {
        group = vim.api.nvim_create_augroup("user-telescope-startup", { clear = true }),
        nested = true,
        callback = function()
          if vim.fn.argc() ~= 0 then
            return
          end

          vim.schedule(function()
            vim.cmd("Telescope find_files")
          end)
        end,
      })
    end,
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
      },
    },
    keys = {
      {
        "<leader><leader>",
        function()
          require("telescope.builtin").find_files({
            cwd = require("config.project-root").current(),
            hidden = false,
            no_ignore = false,
          })
        end,
        desc = "Find files",
      },
      {
        "<leader>ff",
        function()
          require("telescope.builtin").find_files({
            cwd = require("config.project-root").current(),
            hidden = false,
            no_ignore = false,
          })
        end,
        desc = "Find files",
      },
      {
        "<leader>f.",
        function()
          require("telescope.builtin").find_files({
            cwd = require("config.project-root").current(),
            hidden = true,
            no_ignore = false,
            file_ignore_patterns = {
              "^%.git/",
            },
          })
        end,
        desc = "Find hidden files",
      },
      {
        "<leader>fg",
        function()
          require("telescope.builtin").live_grep({
            cwd = require("config.project-root").current(),
            additional_args = function()
              return {
                "--glob",
                "!**/.*",
              }
            end,
          })
        end,
        desc = "Live grep",
      },
      {
        "<leader>fb",
        function()
          require("telescope.builtin").buffers()
        end,
        desc = "Buffers",
      },
      {
        "<leader>fh",
        function()
          require("telescope.builtin").help_tags()
        end,
        desc = "Help tags",
      },
      {
        "<leader>fr",
        function()
          require("telescope.builtin").oldfiles()
        end,
        desc = "Recent files",
      },
      {
        "<leader>fs",
        function()
          require("telescope.builtin").lsp_document_symbols()
        end,
        desc = "Document symbols",
      },
      {
        "<leader>fS",
        function()
          require("telescope.builtin").lsp_workspace_symbols()
        end,
        desc = "Workspace symbols",
      },
    },
    opts = function()
      local actions = require("telescope.actions")

      return {
        defaults = {
          sorting_strategy = "ascending",
          layout_config = {
            prompt_position = "top",
            horizontal = {
              preview_width = 0.55,
            },
          },
          mappings = {
            i = {
              ["<C-j>"] = actions.move_selection_next,
              ["<C-k>"] = actions.move_selection_previous,
              ["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
            },
          },
        },
        pickers = {
          find_files = {
            hidden = true,
          },
          live_grep = {
            additional_args = function()
              return { "--hidden" }
            end,
          },
        },
        extensions = {
          fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
          },
        },
      }
    end,
    config = function(_, opts)
      local telescope = require("telescope")
      telescope.setup(opts)
      pcall(telescope.load_extension, "fzf")
    end,
  },
}
