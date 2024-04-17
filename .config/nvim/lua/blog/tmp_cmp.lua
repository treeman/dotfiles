print("LOADED")

local source = {}

function source:complete(params, callback)
	P(params.context)
	local cursor_before_line = params.context.cursor_before_line

	-- Regex matches `](/`.
	-- The `[^%)]*$` ensures that we continue completion even
	-- after we've typed some characters.
	if string.match(cursor_before_line, "%]%(/[^%)]*$") then
		local items = {
			{
				-- Text to be displayed in the completion menu.
				label = "Rewriting my Neovim config in Lua",
				-- Text to insert.
				insertText = "/blog/2023/10/01/rewriting_my_neovim_config_in_lua/",
				-- Text to filter against, works like `ordinal` for telescope.
				filterText = "/blog/2023/10/01/rewriting_my_neovim_config_in_lua/|Rewriting my Neovim config in Lua",
			},
		}
		callback(items)
	else
		-- `callback` should always be called.
		callback({})
	end
end

-- Trigger completion on these characters.
-- We could also trigger it manually.
function source:get_trigger_characters()
	return { "/" }
end

require("cmp").register_source("blog", source)
