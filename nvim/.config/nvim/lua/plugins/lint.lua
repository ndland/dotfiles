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
        javascript = { "eslint" },
        javascriptreact = { "eslint" },
        markdown = { "vale" },
        text = { "vale" },
        typescript = { "eslint" },
        typescriptreact = { "eslint" },
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
        pattern = {
          "*.md",
          "*.markdown",
          "*.txt",
        },
        callback = function()
          lint.try_lint("vale")
        end,
      })

      local eslint_filetypes = {
        javascript = true,
        javascriptreact = true,
        typescript = true,
        typescriptreact = true,
      }

      local eslint_binary = vim.fn.has("win32") == 1 and "eslint.cmd" or "eslint"

      local function eslint_cwd(bufnr)
        local filename = vim.api.nvim_buf_get_name(bufnr)

        if filename == "" then
          return nil
        end

        local directory = vim.fs.dirname(filename)

        while directory do
          local binary = vim.fs.joinpath(directory, "node_modules", ".bin", eslint_binary)

          if vim.uv.fs_stat(binary) then
            return directory
          end

          local parent = vim.fs.dirname(directory)

          if not parent or parent == directory then
            break
          end

          directory = parent
        end

        return nil
      end

      local function try_eslint(bufnr)
        if not eslint_filetypes[vim.bo[bufnr].filetype] then
          return
        end

        local cwd = eslint_cwd(bufnr)

        if not cwd then
          return
        end

        vim.api.nvim_buf_call(bufnr, function()
          lint.try_lint("eslint", { cwd = cwd })
        end)
      end

      local eslint_augroup = vim.api.nvim_create_augroup("eslint_lint", { clear = true })

      vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave" }, {
        group = eslint_augroup,
        callback = function(args)
          try_eslint(args.buf)
        end,
      })
    end,
  },
}
