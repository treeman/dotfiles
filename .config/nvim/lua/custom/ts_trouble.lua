local Item = require("trouble.item")

local source = {}

-- TODO
-- * Jump to item is still broken
-- * Replace manual heading impl with `ts_headings`

source.config = {
  modes = {
    treesitter = {
      -- events = { "BufEnter", "BufWritePost" },
      source = "treesitter",
      format = "{ts_type} {pos}",
      focus = false,
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
    ts_headings = {
      mode = "treesitter",
      format = "{heading} {pos}",
      filter = function(items)
        local res = {}
        local heading_levels = {}
        for _, item in ipairs(items) do
          local heading_level = tonumber(string.match(item.node:type(), "^heading(%d+)"))
          if heading_level then
            table.insert(res, item)

            if heading_level ~= 1 then
              local parent = heading_levels[heading_level - 1]
              if parent then
                parent:add_child(item)
              end
            end
            heading_levels[heading_level] = item
          end
        end

        return res
      end,
    },
    lists = {
      mode = "treesitter",
      filter = function(items)
        return vim.tbl_filter(function(item)
          return item.node:type() == "list"
        end, items)
      end,
    },
  },
  formatters = {
    ts_type = function(ctx)
      return {
        text = ctx.item.node:type(),
      }
    end,
    heading = function(ctx)
      return {
        text = ctx.item.text,
        hl = "ts",
      }
    end,
  },
}

function source.get(cb)
  local parser = vim.treesitter.get_parser()

  if not parser then
    return
  end

  local items = {}
  local to_process = {}

  local function node_to_item(node)
    local row_start, col_start, row_end, col_end = vim.treesitter.get_node_range(node)

    return Item.new({
      id = table.concat({ row_start, col_start, node:type() }, "|"),
      buf = 0,
      source = "treesitter",
      pos = { row_start + 1, col_start },
      end_pos = { row_end + 1, col_end },
      text = vim.treesitter.get_node_text(node, 0),
      node = node,
    })
  end

  local function process()
    if #to_process > 0 then
      local top = table.remove(to_process, #to_process)
      table.insert(items, top.item)
      for i = top.node:named_child_count() - 1, 0, -1 do
        local child = top.node:named_child(i)
        local item = node_to_item(child)
        top.item:add_child(item)

        table.insert(to_process, { node = child, item = item })
      end
      process()
    end
  end

  parser:for_each_tree(function(ts_tree, _)
    local root = ts_tree:root()
    table.insert(to_process, { node = root, item = node_to_item(root) })
    process()
  end)

  cb(items)
end

require("trouble.sources").register("treesitter", source)
