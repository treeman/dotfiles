local cmd = require("util.helpers").create_cmd
local files = require("blog.files")
local server = require("blog.server")

cmd("BlogStart", server.start)
cmd("BlogStop", server.stop)
cmd("BlogRestart", server.restart)
cmd("BlogReconnect", server.reconnect)

cmd("BlogNewDraft", files.new_draft)
cmd("BlogNewSeries", files.new_series)
cmd("BlogNewStatic", files.new_static)
cmd("BlogNewProject", files.new_project)

cmd("BlogPromoteDraft", files.promote_curr_draft)
cmd("BlogDemotePost", files.demote_curr_post)

cmd("BlogPreview", files.open_curr_post_in_browser)

-- cmd("BlogInfo", server.info)
