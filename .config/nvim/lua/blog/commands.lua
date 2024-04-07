local cmd = require("util").create_cmd
local server = require("blog.server")
local content = require("blog.content")

cmd("BlogStart", server.start)
cmd("BlogStop", server.stop)
cmd("BlogRestart", server.restart)
cmd("BlogReconnect", server.reconnect)

-- cmd("BlogPromoteDraft", content.promote_draft)
-- cmd("BlogDemotePost", content.demote_post)

-- cmd("BlogInfo", server.info)
