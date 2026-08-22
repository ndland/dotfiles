local function project_binary(name)
  local suffix = vim.fn.has("win32") == 1 and ".cmd" or ""
  local filename = vim.api.nvim_buf_get_name(0)
  local directory = filename ~= "" and vim.fs.dirname(filename) or vim.fn.getcwd()

  while directory do
    local candidate = vim.fs.joinpath(directory, "node_modules", ".bin", name .. suffix)

    if vim.uv.fs_stat(candidate) then
      return candidate
    end

    local parent = vim.fs.dirname(directory)

    if not parent or parent == directory then
      break
    end

    directory = parent
  end

  error("Project-local " .. name .. " executable not found")
end

return {
  {
    "mfussenegger/nvim-dap",
    version = "0.10.0",
    keys = {
      {
        "<F5>",
        function()
          require("dap").continue()
        end,
        desc = "Debug continue",
      },
      {
        "<F10>",
        function()
          require("dap").step_over()
        end,
        desc = "Debug step over",
      },
      {
        "<F11>",
        function()
          require("dap").step_into()
        end,
        desc = "Debug step into",
      },
      {
        "<S-F11>",
        function()
          require("dap").step_out()
        end,
        desc = "Debug step out",
      },
      {
        "<leader>bb",
        function()
          require("dap").toggle_breakpoint()
        end,
        desc = "Toggle breakpoint",
      },
      {
        "<leader>bB",
        function()
          require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
        end,
        desc = "Conditional breakpoint",
      },
      {
        "<leader>bc",
        function()
          require("dap").continue()
        end,
        desc = "Continue",
      },
      {
        "<leader>bp",
        function()
          require("dap").pause()
        end,
        desc = "Pause",
      },
      {
        "<leader>br",
        function()
          require("dap").repl.open()
        end,
        desc = "Open debug REPL",
      },
      {
        "<leader>bt",
        function()
          require("dap").terminate()
        end,
        desc = "Terminate",
      },
      {
        "<leader>bx",
        function()
          require("dap").clear_breakpoints()
        end,
        desc = "Clear breakpoints",
      },
    },
    config = function()
      local dap = require("dap")
      local mason_root = vim.fn.stdpath("data") .. "/mason"
      local js_debug_server = vim.fs.joinpath(mason_root, "packages/js-debug-adapter/js-debug/src/dapDebugServer.js")
      local node = vim.fn.exepath("node")

      if node == "" then
        error("Node is not resolvable for nvim-dap")
      end

      if vim.fn.filereadable(js_debug_server) ~= 1 then
        error("js-debug-adapter v1.117.0 is missing; install it with " .. ":MasonInstall js-debug-adapter@v1.117.0")
      end

      dap.adapters["pwa-node"] = {
        type = "server",
        host = "127.0.0.1",
        port = "${port}",
        executable = {
          command = node,
          args = {
            js_debug_server,
            "${port}",
          },
        },
      }

      local attach = {
        type = "pwa-node",
        request = "attach",
        name = "Attach to Node process",
        processId = require("dap.utils").pick_process,
        cwd = "${workspaceFolder}",
        sourceMaps = true,
      }

      dap.configurations.javascript = {
        {
          type = "pwa-node",
          request = "launch",
          name = "Launch current JavaScript file",
          program = "${file}",
          cwd = "${workspaceFolder}",
          sourceMaps = true,
          console = "integratedTerminal",
          skipFiles = {
            "<node_internals>/**",
            "${workspaceFolder}/node_modules/**",
          },
        },
        attach,
      }

      dap.configurations.typescript = {
        {
          type = "pwa-node",
          request = "launch",
          name = "Launch current TypeScript file",
          program = "${file}",
          cwd = "${workspaceFolder}",
          runtimeExecutable = function()
            return project_binary("tsx")
          end,
          sourceMaps = true,
          console = "integratedTerminal",
          skipFiles = {
            "<node_internals>/**",
            "${workspaceFolder}/node_modules/**",
          },
        },
        attach,
      }
    end,
  },
}
