local cmd = require("util").create_cmd

cmd("UpdateAll", function()
	vim.cmd("MasonUpdate")
	vim.cmd("TSUpdate")
	vim.cmd("Lazy sync")
end)

cmd("Format", function(args)
	local range = nil
	if args.count ~= -1 then
		local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
		range = {
			start = { args.line1, 0 },
			["end"] = { args.line2, end_line:len() },
		}
	end
	require("conform").format({ async = true, lsp_fallback = true, range = range })
end, { range = true })

cmd("FormatDisable", function(args)
	if args.bang then
		-- FormatDisable! will disable formatting just for this buffer
		vim.b.disable_autoformat = true
	else
		vim.g.disable_autoformat = true
	end
end, {
	desc = "Disable autoformat-on-save",
	bang = true,
})
cmd("FormatEnable", function()
	vim.g.disable_autoformat = false
end, {
	desc = "Re-enable autoformat-on-save",
})

local blog_server = require("blog.server")
cmd("BlogStart", blog_server.start)
cmd("BlogStop", blog_server.stop)
cmd("BlogRestart", blog_server.restart)

-- cmd("Preview", require("blog.preview").preview_curr_buf)
