return {
  {
    "obsidian-nvim/obsidian.nvim",
    version = "*",
    ft = "markdown",
    cmd = { "Obsidian" },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "Saghen/blink.cmp",
    },
    keys = {
      { "<leader>nn", "<cmd>Obsidian new<cr>", desc = "New note" },
      { "<leader>nt", "<cmd>Obsidian today<cr>", desc = "Today note" },
      { "<leader>ny", "<cmd>Obsidian yesterday<cr>", desc = "Yesterday note" },
      { "<leader>nm", "<cmd>Obsidian tomorrow<cr>", desc = "Tomorrow note" },
      { "<leader>nq", "<cmd>Obsidian quick_switch<cr>", desc = "Quick switch notes" },
      { "<leader>ns", "<cmd>Obsidian search<cr>", desc = "Search note contents" },
      { "<leader>nb", "<cmd>Obsidian backlinks<cr>", desc = "Backlinks" },
      { "<leader>nT", "<cmd>Obsidian new_from_template<cr>", desc = "New from template" },
      { "<leader>nw", "<cmd>Obsidian workspace<cr>", desc = "Switch workspace" },
      { "<leader>no", "<cmd>Obsidian open<cr>", desc = "Open in Obsidian" },
    },
    opts = {
      legacy_commands = false,

      workspaces = {
        {
          name = "main",
          path = "/mnt/c/Users/Nick Land/Documents/GitHub/Second-Brain/",
        },
      },

      notes_subdir = "notes",
      new_notes_location = "notes_subdir",

      daily_notes = {
        folder = "05_Daily",
        date_format = "%Y-%m-%d",
        alias_format = "%B %-d, %Y",
        default_tags = { "daily-notes" },
        template = "Daily.md",
      },

      templates = {
        folder = "templates",
      },

      completion = {
        nvim_cmp = false,
        blink = true,
        min_chars = 2,
      },

      picker = {
        name = "telescope.nvim",
      },

      note_id_func = function(title)
        if title ~= nil then
          return title
          :gsub(" ", "-")
          :gsub("[^A-Za-z0-9-]", "")
          :lower()
        else
          return tostring(os.time())
        end
      end,

      frontmatter = {
        func = function(note)
          local out = {
            id = note.id,
            aliases = note.aliases,
            tags = note.tags,
          }

          if note.metadata ~= nil and not vim.tbl_isempty(note.metadata) then
            for k, v in pairs(note.metadata) do
              out[k] = v
            end
          end

          return out
        end,
      },
    },
    config = function(_, opts)
      require("obsidian").setup(opts)

      vim.api.nvim_create_autocmd("FileType", {
        pattern = "markdown",
        callback = function(event)
          local ok, obsidian = pcall(require, "obsidian")
          if not ok then
            return
          end

          vim.keymap.set("n", "gf", function()
            return obsidian.util.gf_passthrough()
          end, {
          buffer = event.buf,
          expr = true,
          noremap = false,
          silent = true,
          desc = "Follow note link",
        })
      end,
    })
  end,
},
}

