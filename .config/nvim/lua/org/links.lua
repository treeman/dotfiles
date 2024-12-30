local ts = require("org.treesitter")

local M = {}

local function collect_captures(query, language)
  query = vim.treesitter.query.parse(language, query)
  local parser = vim.treesitter.get_parser(0, language)

  local res = {}
  for _, tree in ipairs(parser:trees()) do
    local root = tree:root()
    for _, node, _ in query:iter_captures(root, 0) do
      table.insert(res, node)
    end
  end
  return res
end

local function find_links()
  local task_link_query = [[
  (inline_link) @link
  (full_reference_link) @link
  (collapsed_reference_link) @link
  ]]

  return collect_captures(task_link_query, "djot_inline")
end

-- This finds the closest link
local function filter_links_by_nearest_row(links, target_row)
  local curr_row_dist
  local curr_links = {}

  for _, link in ipairs(links) do
    local row_start, _, row_end, _ = vim.treesitter.get_node_range(link)

    local target_dist = math.min(math.abs(target_row - row_start), math.abs(target_row - row_end))

    if not curr_row_dist or target_dist < curr_row_dist then
      curr_row_dist = target_dist
      curr_links = {}
      table.insert(curr_links, link)
    elseif target_dist == curr_row_dist then
      table.insert(curr_links, link)
    end
  end

  return curr_links
end

-- This filters rows on the same row as the target
local function filter_links_by_row(links, target_row)
  local curr_links = {}

  for _, link in ipairs(links) do
    local row_start, _, row_end, _ = vim.treesitter.get_node_range(link)

    if row_start <= target_row and row_end >= target_row then
      table.insert(curr_links, link)
    end
  end

  return curr_links
end

local function filter_links_by_col(links, target_col)
  local curr_col_dist
  local curr_link

  for _, link in ipairs(links) do
    local _, col_start, _, col_end = vim.treesitter.get_node_range(link)

    -- Yeah this isn't maybe exact when links wrap multiple lines
    local target_dist = math.min(math.abs(target_col - col_start), math.abs(target_col - col_end))

    if not curr_col_dist or target_dist <= curr_col_dist then
      curr_link = link
      curr_col_dist = target_dist
    end
  end

  return curr_link
end

M.get_nearest_link = function()
  local links = find_links()

  -- (1, 0)-indexed
  local cursor = vim.api.nvim_win_get_cursor(0)
  local cursor_row = cursor[1] - 1
  local cursor_col = cursor[2]

  local by_row = filter_links_by_row(links, cursor_row)

  if #by_row == 1 then
    return by_row[1]
  end

  return filter_links_by_col(by_row, cursor_col)
end

local function visit_url(url)
  -- If starts with localhost or http:// try to open it in the browser
  if
    vim.startswith(url, "http://")
    or vim.startswith(url, "https://")
    or vim.startswith(url, "localhost")
  then
    vim.notify("Opening " .. url, vim.log.levels.INFO)
    vim.fn.system("xdg-open " .. url)
    return
  end

  -- TODO what if we're in the blog context? I'd like to
  -- convert the url paths to their respective files here.
  -- 1. Use `blog.content list_markup_content()`
  -- 2. Send command to connected blog backend (Almost like `_d` today, but visit link directly)
  -- 3. Send a new "url to path" command to the backend and edit that one

  local file_path

  if vim.startswith(url, "/") then
    file_path = url
  else
    local current_file_path = vim.api.nvim_buf_get_name(0)
    local current_folder = vim.fn.fnamemodify(current_file_path, ":h")
    file_path = current_folder .. "/" .. url
  end

  vim.notify("Opening " .. file_path, vim.log.levels.INFO)
  vim.cmd("edit " .. file_path)
end

local function find_link_def(link_label)
  vim.notify(link_label, vim.log.levels.WARN)

  local link_def_query = [[
    (link_reference_definition) @def
  ]]

  local defs = collect_captures(link_def_query, "djot")
  for _, def in ipairs(defs) do
    local label = ts.get_text(def:named_child(0))

    if label == link_label then
      local dest = ts.get_text(def:named_child(1))
      vim.notify(dest, vim.log.levels.WARN)
      return dest
    end
  end
end

local function get_link_destination(link)
  if link:type() == "inline_link" then
    return ts.get_text(link:child(1), 1, 1)
  end

  if link:type() == "full_reference_link" then
    local label = ts.get_text(link:named_child(1))
    return find_link_def(label)
  end

  if link:type() == "collapsed_reference_link" then
    local label = ts.get_text(link:named_child(0), 1, 1)
    return find_link_def(label)
  end

  vim.notify("Couldn't handle link type: " .. link:type(), vim.log.levels.ERROR)
end

M.visit_nearest_link = function()
  local link = M.get_nearest_link()

  if not link then
    return
  end

  local dest = get_link_destination(link)
  if not dest then
    vim.notify("Couldn't find link destination " .. link:type(), vim.log.levels.ERROR)
    return
  end
  visit_url(dest)
end

local function get_visual_range()
  -- Need to send escape to update visual selection
  local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
  vim.api.nvim_feedkeys(esc, "x", false)

  local start_pos = vim.api.nvim_buf_get_mark(0, "<")
  local end_pos = vim.api.nvim_buf_get_mark(0, ">")

  local last_col = vim.fn.col({ end_pos[1], "$" }) - 1

  -- Convert from 1- to 0-indexes
  start_pos[1] = start_pos[1] - 1
  end_pos[1] = end_pos[1] - 1

  -- Need to select past the end
  end_pos[2] = end_pos[2] + 1

  -- Sometimes end pos is a very large number way past the ending of the line
  end_pos[2] = math.min(end_pos[2], last_col)

  return { start_pos[1], start_pos[2], end_pos[1], end_pos[2] }
end

---@param link string
local function is_url(link)
  if link:find("^https?://") then
    return true
  end

  return false
end

---@param url string
local function transform_url(url)
  url = url:gsub("^https?://localhost:%d+", "", 1)
  return url
end

--- Returns an url if one is found in `*` registry,
--- otherwise returns "".
local function link_url_to_paste()
  local paste_content = vim.fn.getreg("*", true, true)

  -- If there's more line
  if #paste_content ~= 1 then
    return ""
  end

  local link = paste_content[1]

  --- Only paste the link if it's an url,
  --- otherwise open an empty url for editing.
  if is_url(link) then
    return transform_url(link)
  else
    return ""
  end
end

function M.create_link()
  local link = link_url_to_paste()

  -- Only paste in visual selection
  local mode = vim.api.nvim_get_mode().mode
  local is_visual = mode == "v" or mode == "V"
  if not is_visual then
    return
  end

  local selection = get_visual_range()

  local inline = ts.find_node("inline", "djot_inline")
  if not (inline and vim.treesitter.node_contains(inline, selection)) then
    return
  end

  local lines =
    vim.api.nvim_buf_get_text(0, selection[1], selection[2], selection[3], selection[4], {})

  lines[1] = "[" .. lines[1]
  lines[#lines] = lines[#lines] .. "](" .. link .. ")"

  vim.api.nvim_buf_set_text(0, selection[1], selection[2], selection[3], selection[4], lines)

  -- Set cursor at `)`, so we can easily start editing the pasted text if we want to.
  local end_row = selection[3] + 1
  -- End of selection + ]( + link + )
  local end_col = selection[4] + 2 + #link + 1
  vim.api.nvim_win_set_cursor(0, { end_row, end_col })

  if link == "" then
    vim.cmd("startinsert")
  end
end

return M
