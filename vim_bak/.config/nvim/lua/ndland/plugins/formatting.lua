return {
  "stevearc/conform.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local conform = require("conform")

    conform.setup({
      formatters_by_ft = {
        css = { "prettier" },
        html = { "prettier" },
        javascript = { "prettierd", "eslint_d" },
        javascriptreact = { "prettierd", "eslint_d" },
        json = { "prettier" },
        lua = { "stylua" },
        markdown = { "prettier", "markdownlint" },
        typescript = { "prettierd", "eslint_d" },
        typescriptreact = { "prettierd", "eslint_d" },
        yaml = { "prettier" },
      },

      format_on_save = {
        lsp_fallback = true,
        async = false,
        timeout_ms = 1000,
      },
    })

    local wk = require("which-key")

    wk.add({
      { "<leader>m", group = "Conform" },
      mode = { "n", "v" },
      {
        "<leader>mp",
        function()
          conform.format({
            lsp_fallback = true,
            async = false,
            timeout_ms = 1000,
          })
        end,
        desc = "Format file or range (in visual mode)",
      },
    })
  end,
}
