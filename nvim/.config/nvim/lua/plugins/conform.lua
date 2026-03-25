return {
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    keys = {
      {
        "<leader>lf",
        function()
          require("conform").format({
            async = true,
            lsp_fallback = true,
          })
        end,
        desc = "Format buffer",
      },
    },
    opts = {
      notify_on_error = true,
      format_on_save = function(bufnr)
        local ignore_filetypes = { "markdown" }
        if vim.tbl_contains(ignore_filetypes, vim.bo[bufnr].filetype) then
          return nil
        end

        return {
          timeout_ms = 1500,
          lsp_fallback = true,
        }
      end,
      formatters_by_ft = {
        javascript = { "prettierd", "prettier" },
        javascriptreact = { "prettierd", "prettier" },
        typescript = { "prettierd", "prettier" },
        typescriptreact = { "prettierd", "prettier" },
        astro = { "prettierd", "prettier" },
        css = { "prettierd", "prettier" },
        html = { "prettierd", "prettier" },
        json = { "prettierd", "prettier" },
        lua = { "stylua" },
      },
    },
  },
}

