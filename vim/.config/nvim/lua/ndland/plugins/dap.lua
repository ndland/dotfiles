return {
  "mfussenegger/nvim-dap",
  dependencies = {
    "rcarriga/nvim-dap-ui",
    "nvim-neotest/nvim-nio",
  },

  config = function()
    local dap = require("dap")
    local dapui = require("dapui")

    dapui.setup()

    dap.set_log_level("TRACE")

    local keymap = vim.keymap

    keymap.set("n", "<leader>db", dap.toggle_breakpoint, {})
    keymap.set("n", "<leader>dc", dap.continue, {})

    dap.adapters.chrome = {
      type = "executable",
      command = "node",
      args = { os.getenv("HOME") .. "/code/github.com/microsoft/vscode-chrome-debug/out/src/chromeDebug.js" },
    }

    -- Open and close dap-ui automatically
    dap.listeners.after.event_initialized["dapui_config"] = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated["dapui_config"] = function()
      dapui.close()
    end
    dap.listeners.before.event_exited["dapui_config"] = function()
      dapui.close()
    end

    local host_ip = "172.22.176.1"

    dap.configurations.javascriptreact = {
      {
        name = "Launch Chrome",
        type = "chrome",
        request = "attach",
        program = "${file}",
        cwd = vim.fn.getcwd(),
        sourceMaps = true,
        protocol = "inspector",
        port = 9222,
        webRoot = "${workspaceFolder}",
        host = host_ip,
      },
    }

    dap.configurations.typescriptreact = {
      {
        name = "Launch Chrome",
        type = "chrome",
        request = "attach",
        program = "${file}",
        cwd = vim.fn.getcwd(),
        sourceMaps = true,
        protocol = "inspector",
        port = 9222,
        webRoot = "${workspaceFolder}",
        host = host_ip,
      },
    }
  end,
}
