return {
  "williamboman/mason.nvim",
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",
  },
  config = function()
    -- import mason
    local mason = require("mason")

    local mason_lspconfig = require("mason-lspconfig")

    local mason_tool_installer = require("mason-tool-installer")

    -- enable mason and configure icons
    mason.setup({
      ui = {
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗",
        },
      },
    })

    mason_lspconfig.setup({
      automatic_installation = true, -- automatically install servers
      -- list of servers for mason to install
      ensure_installed = {
        "emmet_ls",
        "html",
        "lua_ls",
        "marksman",
        "tailwindcss",
        "ts_ls",
        -- "zk",
      },
    })

    mason_tool_installer.setup({
      ensure_installed = {
        "eslint_d",
        "markdownlint",
        "prettier", -- prettier formatter
        "stylua", -- lua formatter
        -- "zk",
      },
    })
  end,
}
