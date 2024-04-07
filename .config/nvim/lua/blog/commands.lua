local cmd = require("util").create_cmd
local server = require("blog.server")
local files = require("blog.files")

cmd("BlogStart", server.start)
cmd("BlogStop", server.stop)
cmd("BlogRestart", server.restart)
cmd("BlogReconnect", server.reconnect)

cmd("BlogPromoteDraft", files.promote_curr_draft)
cmd("BlogDemotePost", files.demote_curr_post)

-- cmd("BlogInfo", server.info)
