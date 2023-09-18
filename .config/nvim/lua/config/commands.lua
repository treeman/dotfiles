local cmd = require("util").create_cmd

cmd("UpdateAll", function()
	vim.cmd("MasonUpdate")
	vim.cmd("TSUpdate")
	vim.cmd("Lazy sync")
end)

-- cmd("SynGroup", function()
-- 	local s = vim.cmd("echo synID(line('.'), col('.'), 1)")
-- 	local res = vim.cmd("echo synIDattr(" .. s .. ", 'name') . ' -> ' . synIDattr(synIDtrans(" .. s .. "), 'name')")
-- 	print(res)
-- end)

-- Without TreeSitter
-- :echo synIDattr(synID(line('.'), col('.'), 1), 'name')

-- Treesitter
-- :TSHighlightCapturesUnderCursor

-- function! SynGroup()
--     let l:s = synID(line('.'), col('.'), 1)
--     echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
-- endfun
