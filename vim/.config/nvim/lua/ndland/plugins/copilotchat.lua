return {
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    dependencies = {
      { "github/copilot.vim" }, -- or zbirenbaum/copilot.lua
      { "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
    },
    config = function()
      local status_ok, copilotchat = pcall(require, "CopilotChat")
      if not status_ok then
        vim.notify("Failed to load CopilotChat", vim.log.levels.ERROR)
        return
      end
      copilotchat.setup({
        -- your configuration comes here
      })
    end,
    event = "VimEnter", -- Lazy load on VimEnter
  },
}
