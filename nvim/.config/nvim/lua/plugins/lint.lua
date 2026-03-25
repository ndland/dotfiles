return {
  {
    "mfussenegger/nvim-lint",
    event = { "BufReadPre", "BufNewFile" },
    keys = {
      {
        "<leader>nv",
        function()
          require("lint").try_lint("vale")
        end,
        desc = "Vale current note",
      },
    },
    config = function()
      local lint = require("lint")

      lint.linters_by_ft = {
        markdown = { "vale" },
        text = { "vale" },
      }

      local vale = lint.linters.vale

      vale.args = {
        "--output=JSON",
        "--no-exit",
        "--",
      }

      local vale_augroup = vim.api.nvim_create_augroup("vale_lint", { clear = true })

      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        group = vale_augroup,
        pattern = { "*.md", "*.markdown", "*.txt" },
        callback = function()
          lint.try_lint("vale")
        end,
      })
    end,
  },
}

