local cmd = require("util").create_cmd
local server = require("blog.server")
local files = require("blog.files")

cmd("BlogStart", server.start)
cmd("BlogStop", server.stop)
cmd("BlogRestart", server.restart)
cmd("BlogReconnect", server.reconnect)

cmd("BlogNewDraft", files.new_draft)
cmd("BlogNewSeries", files.new_series)
cmd("BlogPromoteDraft", files.promote_curr_draft)
cmd("BlogDemotePost", files.demote_curr_post)

cmd("BlogPreview", files.open_curr_post_in_browser)

-- cmd("BlogInfo", server.info)
