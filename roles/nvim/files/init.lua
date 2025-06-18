vim.o.wrap = false
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.smartcase = true
vim.o.hlsearch = true

vim.g.mapleader = vim.keycode "<Space>"

-- Initialize plugins
require("config.lazy")

