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
          })
        end,
        desc = "Format buffer",
      },
    },
    opts = {
      notify_on_error = true,

      default_format_opts = {
        lsp_format = "fallback",
      },

      format_on_save = function(bufnr)
        local ignore_filetypes = { "markdown" }

        if vim.tbl_contains(ignore_filetypes, vim.bo[bufnr].filetype) then
          return nil
        end

        return {
          timeout_ms = 1500,
        }
      end,

      formatters = {
        stylua = {
          command = vim.fn.expand("~/.local/share/mise/shims/stylua"),
        },
      },

      formatters_by_ft = {
        javascript = { "prettier" },
        javascriptreact = { "prettier" },
        typescript = { "prettier" },
        typescriptreact = { "prettier" },
        astro = { "prettier" },
        css = { "prettier" },
        html = { "prettier" },
        json = { "prettier" },
        lua = { "stylua" },
      },
    },
  },
}
