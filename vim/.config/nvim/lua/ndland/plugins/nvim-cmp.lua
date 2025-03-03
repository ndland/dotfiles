return {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    "hrsh7th/cmp-buffer", -- source for text in buffer
    "hrsh7th/cmp-path", -- source for file system paths
    {
      "L3MON4D3/LuaSnip",
      -- follow latest release.
      version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
      -- install jsregexp (optional!).
      build = "make install_jsregexp",
    },
    {
      "zbirenbaum/copilot-cmp",
      config = function()
        require("copilot_cmp").setup({
          suggestion = { enabled = false },
          panel = { enabled = false },
        })
      end,
    }, -- for GitHub Copilot
    {
      "David-Kunz/cmp-npm",
      dependencies = { "nvim-lua/plenary.nvim" },
      ft = "json",
      config = function()
        require("cmp-npm").setup({})
      end,
    },
    "saadparwaiz1/cmp_luasnip", -- for autocompletion
    "rafamadriz/friendly-snippets", -- useful snippets
    "onsails/lspkind.nvim", -- vs-code like pictograms
  },
  config = function()
    local cmp = require("cmp")
    require("cmp-npm").setup({})

    local luasnip = require("luasnip")

    local tailwindcss_format = require("tailwindcss-colorizer-cmp").formatter

    -- loads vscode style snippets from installed plugins (e.g. friendly-snippets)
    require("luasnip.loaders.from_vscode").lazy_load()

    cmp.setup({
      completion = {
        completeopt = "menu,menuone,preview,noselect",
      },
      snippet = { -- configure how nvim-cmp interacts with snippet engine
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      mapping = cmp.mapping.preset.insert({
        ["<C-k>"] = cmp.mapping.select_prev_item(), -- previous suggestion
        ["<C-j>"] = cmp.mapping.select_next_item(), -- next suggestion
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete(), -- show completion suggestions
        ["<C-e>"] = cmp.mapping.abort(), -- close completion window
        ["<CR>"] = cmp.mapping.confirm({ select = false }),
      }),
      -- sources for autocompletion
      sources = cmp.config.sources({
        { name = "npm", keyword_length = 4 },
        { name = "copilot" },
        { name = "nvim_lsp" },
        { name = "luasnip" }, -- snippets
        { name = "buffer" }, -- text within current buffer
        { name = "path" }, -- file system paths
      }),

      formatting = {
        fields = { "abbr", "kind", "menu" },
        expandable_indicator = true,
        format = function(entry, vim_item)
          if vim.tbl_contains({ "path" }, entry.source.name) then
            local icon, hl_group = require("nvim-web-devicons").get_icon(entry:completion_item().label)
            if icon then
              vim_item.kind = icon
              vim_item.kind_hl_group = hl_group
              vim_item.menu = ({
                npm = "[npm]",
                copilot = "[copilot]",
                nvim_lsp = "[LSP]",
                luasnip = "[Snippet]",
                buffer = "[Buffer]",
                path = "[Path]",
              })[entry.source.name]
              -- vim_item = tailwindcss_format(entry, vim_item)
              return vim_item
            end
          end
          return require("lspkind").cmp_format({ with_text = true })(entry, vim_item)
        end,
      },
    })
  end,
}
