return {
  {
    "mason-org/mason.nvim",
    opts = {
      ui = {
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗",
        },
      },
    },
  },

  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      "mason-org/mason.nvim",
      "neovim/nvim-lspconfig",
    },
    opts = {
      ensure_installed = {
        "astro",
        "cssls",
        "eslint",
        "html",
        "lua_ls",
        "tailwindcss",
        "ts_ls",
      },
      automatic_enable = false,
    },
  },

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "mason-org/mason.nvim",
      "mason-org/mason-lspconfig.nvim",
      "saghen/blink.cmp",
    },
    config = function()
      local capabilities = require("blink.cmp").get_lsp_capabilities()

      vim.diagnostic.config({
        virtual_text = false,
        signs = true,
        update_in_insert = false,
        underline = true,
        severity_sort = true,
        float = {
          border = "rounded",
          source = "if_many",
        },
      })

      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("user-lsp-attach", { clear = true }),

        callback = function(event)
          local bufnr = event.buf
          local map = function(mode, lhs, rhs, desc)
            vim.keymap.set(mode, lhs, rhs, {
              buffer = bufnr,
              silent = true,
              desc = desc,
            })
          end

          map("n", "gd", vim.lsp.buf.definition, "Goto definition")
          map("n", "gD", vim.lsp.buf.declaration, "Goto declaration")
          map("n", "gr", vim.lsp.buf.references, "Goto references")
          map("n", "gi", vim.lsp.buf.implementation, "Goto implementation")
          map("n", "K", vim.lsp.buf.hover, "Hover")

          map("n", "<leader>la", vim.lsp.buf.code_action, "Code action")
          map("n", "<leader>ld", vim.diagnostic.open_float, "Line diagnostics")
          map("n", "<leader>lr", vim.lsp.buf.rename, "Rename symbol")

          map("n", "[d", vim.diagnostic.goto_prev, "Prev diagnostic")
          map("n", "]d", vim.diagnostic.goto_next, "Next diagnostic")
        end,
      })

      vim.lsp.config("ts_ls", {
        capabilities = capabilities,
      })

      vim.lsp.config("eslint", {
        capabilities = capabilities,
      })

      vim.lsp.config("html", {
        capabilities = capabilities,
      })

      vim.lsp.config("cssls", {
        capabilities = capabilities,
      })

      vim.lsp.config("astro", {
        capabilities = capabilities,
      })

      vim.lsp.config("lua_ls", {
        capabilities = capabilities,
        settings = {
          Lua = {
            runtime = { version = "LuaJIT" },
            workspace = {
              library = vim.api.nvim_get_runtime_file("", true),
              checkThirdParty = false,
            },
            format = { enable = false },
            telemetry = { enable = false },
          },
        },
      })

      vim.lsp.config("bashls", {
        capabilities = capabilities,
        filetypes = { "sh", "bash" },
      })

      vim.lsp.config("tailwindcss", {
        capabilities = capabilities,
        root_dir = function(bufnr, on_dir)
          local root = vim.fs.root(bufnr, {
            "tailwind.config.js",
            "tailwind.config.cjs",
            "tailwind.config.ts",
            "postcss.config.js",
            "postcss.config.cjs",
            "postcss.config.ts",
            "package.json",
            ".git",
          })
          if root then
            on_dir(root)
          end
        end,
        settings = {
          tailwindCSS = {
            validate = true,
            lint = {
              cssConflict = "warning",
              invalidApply = "error",
              invalidConfigPath = "error",
              invalidScreen = "error",
              invalidTailwindDirective = "error",
              invalidVariant = "error",
              recommendedVariantOrder = "warning",
            },
          },
        },
      })

      vim.lsp.enable("ts_ls")
      vim.lsp.enable("html")
      vim.lsp.enable("cssls")
      vim.lsp.enable("astro")
      vim.lsp.enable("eslint")
      vim.lsp.enable("tailwindcss")
      vim.lsp.enable("lua_ls")
      vim.lsp.enable("bashls")
    end,
  },
}
