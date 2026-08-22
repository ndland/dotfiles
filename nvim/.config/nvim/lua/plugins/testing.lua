local excluded_dirs = {
  ".git",
  ".next",
  "build",
  "coverage",
  "dist",
  "node_modules",
  "playwright-report",
  "test-results",
}

local function filter_dir(name)
  return not vim.tbl_contains(excluded_dirs, name)
end

local function normalize_path(file_path)
  return file_path:gsub("\\", "/")
end

local function is_javascript_test_file(file_path)
  local path = normalize_path(file_path)

  return path:match("%.test%.[tj]sx?$") ~= nil
    or path:match("%.spec%.[tj]sx?$") ~= nil
end

local function is_e2e_test_file(file_path)
  local path = normalize_path(file_path)
  local in_e2e_dir = path:match("^tests/e2e/") ~= nil
  or path:find("/tests/e2e/", 1, true) ~= nil

  return in_e2e_dir and is_javascript_test_file(path)
end

local function is_vitest_test_file(file_path)
  return is_javascript_test_file(file_path)
    and not is_e2e_test_file(file_path)
end

return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "marilari88/neotest-vitest",
      "thenbe/neotest-playwright",
    },
    keys = {
      {
        "<leader>rn",
        function()
          require("neotest").run.run()
        end,
        desc = "Run nearest test",
      },
      {
        "<leader>rf",
        function()
          require("neotest").run.run(vim.fn.expand("%"))
        end,
        desc = "Run test file",
      },
      {
        "<leader>ra",
        function()
          require("neotest").run.run(vim.fn.getcwd())
        end,
        desc = "Run all tests",
      },
      {
        "<leader>rs",
        function()
          require("neotest").summary.toggle()
        end,
        desc = "Test summary",
      },
      {
        "<leader>ro",
        function()
          require("neotest").output.open({
            enter = true,
            auto_close = true,
          })
        end,
        desc = "Test output",
      },
      {
        "<leader>rO",
        function()
          require("neotest").output_panel.toggle()
        end,
        desc = "Test output panel",
      },
      {
        "<leader>rx",
        function()
          require("neotest").run.stop()
        end,
        desc = "Stop test",
      },
    },
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-vitest")({
            filter_dir = filter_dir,
            is_test_file = is_vitest_test_file,
          }),
          require("neotest-playwright").adapter({
            options = {
              enable_dynamic_test_discovery = false,
              persist_project_selection = false,
              filter_dir = filter_dir,
              is_test_file = is_e2e_test_file,
            },
          }),
        },
      })
    end,
  },
}
