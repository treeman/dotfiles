local source = {}

-- FIXME
-- function source:is_available()
-- 	return require("blog.server").is_connected()
-- end

function source:complete(params, callback)
	callback({
		{ label = "xxx" },
	})
end

function source:get_trigger_characters()
	return { "/" }
end

require("cmp").register_source("blog", source)
