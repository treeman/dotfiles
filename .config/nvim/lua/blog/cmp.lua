local source = {}

-- FIXME
function source:is_available()
	return require("blog.server").is_connected()
end

function source:complete(params, callback)
	-- When to complete what:
	-- Url:
	--	previous char: ( for example: [](/url)
	--	or in link def [label]: / (not sure how to do this?)
	--
	-- Toml prematter:
	-- tags = ["One", "Two"]
	-- series = ""
	P(params)
	callback({
		{ label = "xxx" },
	})
end

function source:get_trigger_characters()
	return { "/" }
end

require("cmp").register_source("blog", source)
