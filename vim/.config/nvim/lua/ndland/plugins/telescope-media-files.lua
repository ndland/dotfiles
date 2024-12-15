return {
  "nvim-telescope/telescope-media-files.nvim",
  "nvim-lua/popup.nvim",
  depedencies = { "nvim-telescope/telescope.nvim", "nvim-lua/popup.nvim", "nvim-lua/plenary.nvim" },
  config = function()
    require("telescope").load_extension("media_files")
  end,
}
