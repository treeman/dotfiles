local server = require("blog.server")

local source = {}

function source:is_available()
	return server.is_connected()
end

local function read_lines(file, start_row, end_row, res)
	local linenum = 1
	for line in io.lines(file) do
		if linenum >= start_row and linenum <= end_row then
			table.insert(res, line)
		end
		linenum = linenum + 1
		if linenum > end_row then
			break
		end
	end
end

local function read_post_body(file, limit, res)
	local count = 1
	local frontmatter_delimiters = 0
	for line in io.lines(file) do
		if frontmatter_delimiters == 2 then
			table.insert(res, line)
			count = count + 1
			if count > limit then
				break
			end
		elseif string.match(line, "^%-%-%-") then
			frontmatter_delimiters = frontmatter_delimiters + 1
		end
	end
end

local function _post_docs(post)
	local tag_line = {}
	for _, tag in ipairs(post.tags) do
		table.insert(tag_line, '"*' .. tag .. '*"')
	end

	local res = {
		"`" .. post.title .. "`",
		post.created,
		vim.fn.join(tag_line, ", "),
	}

	if post.series then
		table.insert(res, "**" .. post.series .. "**")
	end

	table.insert(res, "---")
	read_post_body(post.path, 20, res)

	return res
end

local function _standalone_docs(standalone)
	local res = {
		"`" .. standalone.title .. "`",
		"---",
	}
	read_post_body(standalone.path, 20, res)

	return res
end

local function _constant_docs(standalone)
	local res = {
		"`" .. standalone.title .. "` hardcoded",
	}

	return res
end

local function _series_docs(series)
	local res = {
		"`" .. series.title .. "` *" .. series.id .. "*",
		"---",
	}
	read_post_body(series.path, 20, res)
	table.insert(res, "---")
	for _, post in ipairs(series.posts) do
		table.insert(res, "- **" .. post.title .. "**")
		table.insert(res, "  " .. post.created)
	end

	return res
end

local function _tag_docs(tag)
	local res = {
		#tag.posts .. " tagged `" .. tag.name .. "`",
		"---",
	}
	for _, post in ipairs(tag.posts) do
		table.insert(res, "- **" .. post.title .. "**")
		table.insert(res, "  " .. post.created)
	end

	return res
end

local function _img_docs(img)
	return {
		"`" .. img.url .. "`",
	}
end

local function _heading_docs(heading)
	if heading.context.path then
		local res = {
			"*" .. heading.context.url .. "*",
			"---",
		}
		read_lines(heading.context.path, heading.context.start_row, heading.context.end_row + 10, res)
		return res
	else
		return vim.api.nvim_buf_get_lines(0, heading.context.start_row, heading.context.end_row + 10, false)
	end
end

local function _link_def_docs(def)
	-- NOTE this sometimes fail as the def might be starting on the next line at the beginning
	-- return vim.api.nvim_buf_get_lines(0, def.start_row, def.end_row + 1, false)
	return { "[" .. def.label .. "]: " .. def.url }
end

local function _broken_link_docs(link)
	return vim.api.nvim_buf_get_lines(0, link.row - 1, link.row + 1, false)
end

function source:resolve(item, callback)
	if item.info then
		local lines

		if item.info.type == "Post" then
			lines = _post_docs(item.info)
		elseif item.info.type == "Standalone" then
			lines = _standalone_docs(item.info)
		elseif item.info.type == "Constant" then
			lines = _constant_docs(item.info)
		elseif item.info.type == "Series" then
			lines = _series_docs(item.info)
		elseif item.info.type == "Tag" then
			lines = _tag_docs(item.info)
		elseif item.info.type == "Img" then
			lines = _img_docs(item.info)
		elseif item.info.type == "Heading" then
			lines = _heading_docs(item.info)
		elseif item.info.type == "LinkDef" then
			lines = _link_def_docs(item.info)
		elseif item.info.type == "BrokenLink" then
			lines = _broken_link_docs(item.info)
		end

		if lines and #lines > 0 then
			item.documentation = {
				kind = "markdown",
				value = vim.fn.join(lines, "\n") .. "\n",
			}
		end
	end

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
end

function source:get_trigger_characters()
	return { "/", '"', "[", " ", "(", "#" }
end

require("cmp").register_source("blog", source)
