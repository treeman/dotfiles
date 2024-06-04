local Item = require("trouble.item")

-- local heading_query = [[
-- ;;query
-- (heading1 (content) @heading)
-- (heading2 (content) @heading)
-- (heading3 (content) @heading)
-- (heading4 (content) @heading)
-- (heading5 (content) @heading)
-- (heading6 (content) @heading)
-- ]]

local function get_headings()
  local parser = vim.treesitter.get_parser(0, "djot")

  local res = {}

  -- local query = vim.treesitter.query.parse("djot", heading_query)
  local highlights = vim.treesitter.query.get("djot", "highlights")
  if highlights then
    parser:for_each_tree(function(parent_tree, _)
      local root = parent_tree:root()
      for _, node, _ in highlights:iter_captures(root, 0) do
        local row_start, col_start, row_end, col_end = vim.treesitter.get_node_range(node)

        local heading_level = string.match(node:type(), "^heading(%d+)")
        if heading_level then
          table.insert(res, {
            text = vim.treesitter.get_node_text(node, 0),
            type = node:type(),
            level = tonumber(heading_level),
            pos = { row_start + 1, col_start },
            end_pos = { row_end + 1, col_end },
          })
        end
      end
    end)
  end
  return res
end

local source = {}

source.config = {
  modes = {
    headings = {
      -- events = { "BufEnter", "BufWritePost" },
      source = "headings",
      format = "{heading} {row}",
      focus = false,
      -- FIXME cursor following just doesn't work when moving cursor in main win.
      follow = true,
      auto_preview = false,
      win = { position = "right", size = 50 },
      groups = {
        { "filename", format = "{file_icon} {filename} {count}" },
      },
      events = {
        "BufEnter",
        { event = "TextChanged", main = true },
        { event = "CursorMoved", main = true },
      },
    },
  },
  formatters = {
    heading = function(ctx)
      return {
        text = ctx.item.text,
        hl = "ts.djot",
      }
    end,
    row = function(ctx)
      return {
        -- text = tostring(ctx.item.pos[1]),
        text = ctx.item.pos[1] .. ", " .. ctx.item.end_pos[1],
        -- text = ctx.item.pos[1]
        --   .. ","
        --   .. ctx.item.pos[2]
        --   .. " "
        --   .. ctx.item.end_pos[1]
        --   .. ","
        --   .. ctx.item.end_pos[2],
        hl = "LineNr",
      }
    end,
  },
}

function source.get(cb)
  local headings = get_headings()

  local heading_levels = {}
  local res = {}
  for _, heading in ipairs(headings) do
    local item = Item.new({
      pos = heading.pos,
      end_pos = heading.end_pos,
      buf = 0,
      type = heading.type,
      filename = vim.api.nvim_buf_get_name(0),
      item = heading,
      source = "headings",
    })

    if heading.level ~= 1 then
      local parent = heading_levels[heading.level - 1]
      if parent then
        parent:add_child(item)
      end
    end
    heading_levels[heading.level] = item
    table.insert(res, item)
  end
  cb(res)
end

require("trouble.sources").register("headings", source)
