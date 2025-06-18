return {
  { 'nvim-telescope/telescope.nvim', tag = '0.1.8',
    keys = {
      { "<leader><Space>", function() require('telescope.builtin').find_files() end, desc="Find files" },
      { "<leader>ff", function() require('telescope.builtin').find_files() end, desc="Find files" },
      { "<leader>bb", function() require('telescope.builtin').buffers() end, desc="List buffers" },
    },
  },
}
