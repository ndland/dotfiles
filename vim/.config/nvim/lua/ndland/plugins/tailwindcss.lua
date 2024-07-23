return {
  "mrshmllow/document-color.nvim",
  config = function()
    local document_color = require("document-color")

    document_color.setup({
      -- Default options
      mode = "foreground", -- "background" | "foreground" | "single"
    })
  end,
}
