local server = require("blog.server")
local content = require("blog.content")
local path = require("blog.path")

-- FIXME temporary
-- require("blog.tmp_cmp")

local source = {}

function source:is_available()
	return server.is_connected()
end

function source:resolve(item, callback)
	-- TODO should fill in `documentation` I guess
	P(item)
	callback(item)
end

function source:complete(params, callback)
	server.call({
		id = "Complete",
		path = vim.fn.expand("%:p"),
		cursor_before_line = params.context.cursor_before_line,
		col = params.context.cursor.col,
		row = params.context.cursor.row,
	}, function(reply)
		callback(reply.completion_items)
	end)
	-- local cursor_line = params.context.cursor_line
	-- local cursor_before_line = params.context.cursor_before_line

	-- -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/
	-- -- https://github.com/hrsh7th/nvim-cmp/blob/main/lua/cmp/types/lsp.lua
	-- -- kinds:
	-- -- export const Text = 1;
	-- -- export const Method = 2;
	-- -- export const Function = 3;
	-- -- export const Constructor = 4;
	-- -- export const Field = 5;
	-- -- export const Variable = 6;
	-- -- export const Class = 7;
	-- -- export const Interface = 8;
	-- -- export const Module = 9;
	-- -- export const Property = 10;
	-- -- export const Unit = 11;
	-- -- export const Value = 12;
	-- -- export const Enum = 13;
	-- -- export const Keyword = 14;
	-- -- export const Snippet = 15;
	-- -- export const Color = 16;
	-- -- export const File = 17;
	-- -- export const Reference = 18;
	-- -- export const Folder = 19;
	-- -- export const EnumMember = 20;
	-- -- export const Constant = 21;
	-- -- export const Struct = 22;
	-- -- export const Event = 23;
	-- -- export const Operator = 24;
	-- -- export const TypeParameter = 25;

	-- -- Expand images separately because I only ever use it in a `![](/url)`
	-- -- context and not mixing with other urls gives a more pleasant experience.
	-- if string.match(cursor_before_line, "!%[%]%([^%)]*$") then
	-- 	content.list_images(function(imgs)
	-- 		local res = {}
	-- 		for _, img in ipairs(imgs) do
	-- 			table.insert(res, {
	-- 				label = "/" .. path.rel_path(img),
	-- 				kind = 17,
	-- 			})
	-- 		end
	-- 		callback(res)
	-- 	end)
	-- 	return
	-- end

	-- -- Two cases for expanding urls:
	-- -- 1. Expanding inline links, e.g. `[txt](/`					<- expand
	-- -- 2. Expanding links in ref defs, e.g. `[label]: `   <- expand
	-- local expand_url = string.match(cursor_before_line, "^%[.+%]:%s+")
	-- 	or string.match(cursor_before_line, "%]%(/[^%)]*$")
	-- if expand_url then
	-- 	content.list_urls(function(reply)
	-- 		local res = {}
	-- 		for _, info in ipairs(reply.urls) do
	-- 			table.insert(res, {
	-- 				label = info.title,
	-- 				insertText = info.url,
	-- 				filterText = info.url .. info.title,
	-- 				detail = info.url,
	-- 				kind = 17,
	-- 			})
	-- 		end
	-- 		callback(res)
	-- 	end)
	-- 	return
	-- end

	-- -- Expand headings for current file:
	-- -- 1. Expanding inline links, e.g. `[txt](#`					 <- expand
	-- -- 2. Expanding links in ref defs, e.g. `[label]: #`   <- expand
	-- local expand_curr_heading = string.match(cursor_before_line, "^%[.+%]:%s+#")
	-- 	or string.match(cursor_before_line, "%]%(#[^%)]*$")
	-- if expand_curr_heading then
	-- 	content.list_headings_in_curr(function(reply)
	-- 		P(reply)
	-- 		local res = {}
	-- 		for _, info in ipairs(reply.headings) do
	-- 			table.insert(res, {
	-- 				label = info.content,
	-- 				insertText = info.id,
	-- 				filterText = info.content,
	-- 				kind = 7,
	-- 			})
	-- 		end
	-- 		callback(res)
	-- 	end)
	-- 	return
	-- end

	-- -- Expand url definition tags in `[text][tag]`
	-- if string.match(cursor_before_line, "%[[^%]]+%]%[[^%]]*$") then
	-- 	content.list_link_defs(function(reply)
	-- 		local res = {}
	-- 		for _, info in ipairs(reply.defs) do
	-- 			table.insert(res, {
	-- 				label = info.label,
	-- 				insertText = info.label,
	-- 				filterText = info.url .. info.label,
	-- 				detail = info.url,
	-- 				kind = 18,
	-- 			})
	-- 		end

	-- 		callback(res)
	-- 	end)
	-- 	return
	-- end

	-- -- Expand url definition tags in `[tag][]`, simplified to after a `[`.
	-- if string.match(cursor_before_line, "%[[^%]]*$") then
	-- 	content.list_link_defs(function(reply)
	-- 		local res = {}
	-- 		for _, info in ipairs(reply.defs) do
	-- 			table.insert(res, {
	-- 				label = info.label,
	-- 				insertText = info.label,
	-- 				filterText = info.url .. info.label,
	-- 				detail = info.url,
	-- 				kind = 18,
	-- 			})
	-- 		end

	-- 		-- Also includes short headings that are specified with the heading content.
	-- 		content.list_headings_in_curr(function(heading_reply)
	-- 			for _, info in ipairs(heading_reply.headings) do
	-- 				table.insert(res, {
	-- 					label = info.content,
	-- 					kind = 7,
	-- 				})
	-- 			end

	-- 			-- If we start at the beginning of a line, we should complete
	-- 			-- broken link tags as well.
	-- 			if string.match(cursor_before_line, "^%[") then
	-- 				content.list_broken_links(function(broken_links)
	-- 					for _, info in ipairs(broken_links.links) do
	-- 						table.insert(res, {
	-- 							label = info.tag,
	-- 							detail = "Broken link",
	-- 							kind = 18,
	-- 						})
	-- 					end
	-- 					callback(res)
	-- 				end)
	-- 			else
	-- 				callback(res)
	-- 			end
	-- 		end)
	-- 	end)
	-- 	return
	-- end

	-- -- Expand tags:
	-- -- If line starts with `tags = ` and last char is "
	-- if string.match(cursor_line, "^tags = ") then
	-- 	content.list_tags(function(reply)
	-- 		local res = {}
	-- 		for _, entity in ipairs(reply.tags) do
	-- 			table.insert(res, {
	-- 				label = entity.name,
	-- 				commitCharacters = { '"' },
	-- 				kind = 18,
	-- 			})
	-- 		end
	-- 		callback(res)
	-- 	end)
	-- 	return
	-- end

	-- -- Expand series:
	-- -- `series = "`		<- expand
	-- if string.match(cursor_before_line, '^series = "') then
	-- 	content.list_series(function(reply)
	-- 		local res = {}
	-- 		for _, entity in ipairs(reply.series) do
	-- 			local filter_text = entity.title .. entity.id
	-- 			for _, post in ipairs(entity.posts) do
	-- 				filter_text = filter_text .. post.title
	-- 			end

	-- 			table.insert(res, {
	-- 				label = entity.title,
	-- 				insertText = entity.id,
	-- 				filterText = filter_text,
	-- 				commitCharacters = { '"' },
	-- 				kind = 18,
	-- 			})
	-- 		end
	-- 		callback(res)
	-- 	end)
	-- 	return
	-- end

	-- -- TODO autocomplete external heading refs like `/some/path#my-id`
	-- -- TODO autocomplete footnote refs `[^ref]

	-- -- Should always call the callback.
	-- callback({})
end

function source:get_trigger_characters()
	return { "/", '"', "[", " ", "(", "#" }
end

require("cmp").register_source("blog", source)

-- P(string.match("(/", "%(/$|^%[.+%]:%s+$"))
-- P(string.match("/", "%(/$"))
-- P(string.match("[x]: ", "^%[.+%]:%s+$"))

-- server.list_urls(function(urls)
-- 	P(urls)
-- end)

-- P(string.match("x ![](", "!%[%]%($"))
