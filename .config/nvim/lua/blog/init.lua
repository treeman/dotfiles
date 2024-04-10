require("blog.path")
require("blog.server")
require("blog.cmp")
require("blog.commands")
require("blog.autocmd")

-- Keymaps to define
-- Always:
-- list posts
-- list drafts
-- create new draft
--
-- In blog markup file:
-- list tags
-- open current file in firefox
-- `help`
--   e.g. Floating window preview link under cursor
-- `goto`
--   - jump to link under cursor
--   - jump to def under cursor
--
-- Commands:
-- demote current file
-- promote current file
-- (make a commands.lua file)

-- TODO user commands
-- + Open current file in firefox
-- + Control preview?
--
-- TODO jump to
-- + link labels
-- + other posts

-- TODO server commands
-- + refresh
-- + goto def
-- + get headers in post
