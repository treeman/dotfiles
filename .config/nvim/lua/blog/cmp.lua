local server = require("blog.server")
local content = require("blog.content")
local nio = require("nio")

local source = {}

-- FIXME
function source:is_available()
	return server.is_connected()
end

function source:complete(params, callback)
	local cursor_line = params.context.cursor_line
	local cursor_before_line = params.context.cursor_before_line

	-- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/
	-- https://github.com/hrsh7th/nvim-cmp/blob/main/lua/cmp/types/lsp.lua
	-- kinds:
	--	export const Text = 1;
	--	export const File = 17;
	--	export const Reference = 18;

	-- Two cases for expanding urls:
	-- 1. Expanding inline links, e.g. `[txt](/`					<- expand
	-- 2. Expanding links in ref defs, e.g. `[label]: `   <- expand
	local expand_url = string.match(cursor_before_line, "^%[.+%]:%s+$") or string.match(cursor_before_line, "%]%(/$")
	if expand_url then
		content.list_urls(function(reply)
			local res = {}
			for _, info in ipairs(reply.urls) do
				table.insert(res, {
					label = info.title,
					insertText = info.url,
					filterText = info.url .. info.title,
					detail = info.url,
					kind = 17,
				})
			end
			callback(res)
		end)
		return
	end

	-- Expand tags:
	-- If line starts with `tags = ` and last char is "
	if string.match(cursor_line, "^tags = ") and string.match(cursor_before_line, '"$') then
		content.list_tags(function(reply)
			local res = {}
			for _, entity in ipairs(reply.tags) do
				table.insert(res, {
					label = entity.name,
					commitCharacters = { '"' },
					kind = 18,
				})
			end
			callback(res)
		end)
		return
	end

	-- Expand series:
	-- `series = "`		<- expand
	if string.match(cursor_before_line, '^series = "') then
		content.list_series(function(reply)
			local res = {}
			for _, entity in ipairs(reply.series) do
				table.insert(res, {
					label = entity.name,
					commitCharacters = { '"' },
					kind = 18,
				})
			end
			callback(res)
		end)
		return
	end

	-- TODO should complete images

	-- TODO autocomplete heading refs `[Heading text][]` and `/some/path#my-id`
	-- TODO autocomplete link refs `[ref][]` and `[descr][ref]`
	-- TODO autocomplete footnote refs `[^ref]
end

function source:get_trigger_characters()
	return { "/", '"', "[", " " }
end

require("cmp").register_source("blog", source)

-- P(string.match("(/", "%(/$|^%[.+%]:%s+$"))
-- P(string.match("/", "%(/$"))
-- P(string.match("[x]: ", "^%[.+%]:%s+$"))

-- server.list_urls(function(urls)
-- 	P(urls)
-- end)
