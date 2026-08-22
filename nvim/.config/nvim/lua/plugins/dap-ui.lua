return {
  {
    "rcarriga/nvim-dap-ui",
    commit = "cc9dd33aade7f20bae414d0cba163bc60d4d4b43",
    lazy = false,
    dependencies = {
      "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
    },
    keys = {
      {
        "<leader>bu",
        function()
          require("dapui").toggle()
        end,
        desc = "Toggle debug UI",
      },
      {
        "<leader>bh",
        function()
          require("dapui").eval()
        end,
        desc = "Debug hover",
      },
      {
        "<leader>bw",
        function()
          require("dapui").elements.watches.add()
        end,
        desc = "Add debug watch",
      },
    },
    opts = {
      controls = {
        enabled = false,
      },
      floating = {
        border = "single",
      },
      layouts = {
        {
          elements = {
            {
              id = "scopes",
              size = 0.4,
            },
            {
              id = "breakpoints",
              size = 0.15,
            },
            {
              id = "stacks",
              size = 0.25,
            },
            {
              id = "watches",
              size = 0.2,
            },
          },
          position = "left",
          size = 40,
        },
      },
    },
    config = function(_, opts)
      require("dapui").setup(opts)
    end,
  },
}
