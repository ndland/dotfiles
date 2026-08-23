vim.g.mapleader = " "
vim.g.maplocalleader = " "

local map = vim.keymap.set

map({ "n", "v" }, "<leader>y", [["+y]], { desc = "Yank to clipboard" })
map("n", "<leader>Y", [["+Y]], { desc = "Yank line to clipboard" })
map({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete to blackhole" })

local agent_context = require("config.agent-context")

map("n", "<leader>aa", agent_context.jump_to_agent, { desc = "Agent: jump to chat" })
map("n", "<leader>ac", agent_context.add_current_file, { desc = "Agent: add current file" })
map("x", "<leader>ac", agent_context.add_selection, { desc = "Agent: add selection" })
