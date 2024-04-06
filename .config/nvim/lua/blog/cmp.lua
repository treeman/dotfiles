local server = require("blog.server")
local nio = require("nio")

local source = {}

-- FIXME
function source:is_available()
	return server.is_connected()
end

function source:complete(params, callback)
	-- P(params)
	-- When to complete what:
	-- Url:
	--	previous char: ( for example: [](/url)
	--	or in link def [label]: / (not sure how to do this?)
	--
	-- Toml prematter:
	-- tags = ["One", "Two"]
	-- series = ""
	-- P(params)
	-- P(params.context.cursor_before_line)

	local cursor_before_line = params.context.cursor_before_line
	P(cursor_before_line)

	-- if last char is `/`, trigger url

	server.list_urls(function(reply)
		local res = {}
		for _, url in ipairs(reply.urls) do
			-- insertText
			-- filterText
			-- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/
			-- https://github.com/hrsh7th/nvim-cmp/blob/main/lua/cmp/types/lsp.lua
			-- kinds:
			--	export const Text = 1;
			--	export const File = 17;
			--	export const Reference = 18;
			table.insert(res, { label = url, filterText = "xxx" })
		end
		callback(res)
	end)
end

function source:get_trigger_characters()
	return { "/", '"', "[" }
end

require("cmp").register_source("blog", source)

-- server.list_urls(function(urls)
-- 	P(urls)
-- end)
