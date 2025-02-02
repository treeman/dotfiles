local ts = require("org.treesitter")

local M = {}

local function find_links()
  local query = [[
  (inline_link) @link
  (full_reference_link) @link
  (collapsed_reference_link) @link
  (autolink) @link
  ]]

  return ts.collect_captures(query)
end

local function find_link_defs()
  local query = [[
  (link_reference_definition) @def
  ]]

  return ts.collect_captures(query)
end

---@return TSNode | nil
function M.get_nearest_link()
  return ts.get_nearest_node(find_links())
end

function M.get_nearest_link_def()
  return ts.get_nearest_node(find_link_defs())
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
  local link_def_query = [[
    (link_reference_definition) @def
  ]]

  local defs = ts.collect_captures(link_def_query)
  for _, def in ipairs(defs) do
    local label = ts.get_text(def:named_child(0))

    if label == link_label then
      return def
    end
  end
end

local function get_link_def_url_range(link_label)
  local def = find_link_def(link_label)

  if def then
    return ts.get_range(def:named_child(1))
  end
end

local function get_link_destination_range(link)
  if link:type() == "inline_link" then
    return ts.get_range(link:field("destination")[1], 1, 1)
  end

  if link:type() == "full_reference_link" then
    local label = ts.get_text(link:field("label")[1])
    return get_link_def_url_range(label)
  end

  if link:type() == "collapsed_reference_link" then
    local label = ts.get_text(link:field("text")[1], 1, 1)
    return get_link_def_url_range(label)
  end

  if link:type() == "autolink" then
    return ts.get_range(link, 1, 1)
  end

  if link:type() == "link_reference_definition" then
    return ts.get_range(link:field("destination")[1])
  end
end

local function get_link_destination(link)
  local row_start, col_start, row_end, col_end = get_link_destination_range(link)
  if row_start then
    return vim.api.nvim_buf_get_text(0, row_start, col_start, row_end, col_end, {})[1]
  end
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

local function change_selection(selection, cb)
  local lines =
      vim.api.nvim_buf_get_text(0, selection[1], selection[2], selection[3], selection[4], {})

  local res = cb(lines)

  vim.api.nvim_buf_set_text(0, selection[1], selection[2], selection[3], selection[4], lines)

  return res
end

local function create_inline_link(link, selection)
  change_selection(selection, function(lines)
    local res = vim.fn.join(lines, "\n")
    lines[1] = "[" .. lines[1]
    lines[#lines] = lines[#lines] .. "](" .. link .. ")"
    return res
  end)

  -- Set cursor at `)`, so we can easily start editing the pasted text if we want to.
  local end_row = selection[3] + 1
  -- End of selection + ]( + link + )
  local end_col = selection[4] + 2 + #link + 1
  vim.api.nvim_win_set_cursor(0, { end_row, end_col })

  if link == "" then
    vim.cmd("startinsert")
  end
end

local function find_row_to_insert_link_def()
  local line_count = vim.api.nvim_buf_line_count(0)

  local has_new_line = false
  for i = line_count, 1, -1 do
    local line = vim.api.nvim_buf_get_lines(0, i - 1, i, false)[1]
    if line and line ~= "" then
      -- FIXME no longer accepts a position...
      local def =
          ts.find_node_from_cursor("link_reference_definition", { lang = "djot", pos = { i - 1, 0 } })
      if def then
        -- If the last non-empty line is a link definition, we should insert it directly below
        return i + 1
      else
        -- Otherwise add an empty space if needed
        if has_new_line then
          return i + 1
        else
          return i + 2
        end
      end
    else
      -- We go from the bottom up and will return as soon as we find a non-empty line.
      has_new_line = true
    end
  end

  return line_count
end

local function insert_link_def(label, link)
  local link_def_row = find_row_to_insert_link_def()
  local line_count = vim.api.nvim_buf_line_count(0)

  local replacement = { "[" .. label .. "]: " .. link }
  local end_row = link_def_row
  if link_def_row >= line_count + 2 then
    table.insert(replacement, 1, "")
    end_row = end_row + 1
  end

  vim.api.nvim_buf_set_lines(0, link_def_row, link_def_row, false, replacement)
end

local function set_cursor_at_last_content()
  local line_count = vim.api.nvim_buf_line_count(0)
  for i = line_count, 1, -1 do
    local line = vim.api.nvim_buf_get_lines(0, i - 1, i, false)[1]
    if line and line ~= "" then
      vim.api.nvim_win_set_cursor(0, { i, 0 })
      break
    end
  end
  vim.cmd("startinsert!")
end

local function create_collapsed_reference_link(link, selection)
  local label = change_selection(selection, function(lines)
    local res = vim.fn.join(lines, "\n")
    lines[1] = "[" .. lines[1]
    lines[#lines] = lines[#lines] .. "][]"
    return res
  end)

  insert_link_def(label, link)

  if link == "" then
    -- If link is empty place cursor at definition and start inserting
    set_cursor_at_last_content()
  else
    -- Set cursor at the end of `][]`.
    local end_row = selection[3] + 1
    -- End of selection + `][]
    local end_col = selection[4] + 3
    vim.api.nvim_win_set_cursor(0, { end_row, end_col })
  end
end

-- Create a slug from a title.
local function slugify(title)
  title = title:lower()
  title = title:gsub("[^ a-zA-Z0-9_-]+", "")
  title = title:gsub("[ _]+", "-")
  title = title:gsub("^[ _-]+", "")
  title = title:gsub("[ _-]+$", "")
  return title
end

local function create_full_reference_link(link, selection)
  local label = change_selection(selection, function(lines)
    local label = slugify(vim.fn.join(lines, "\n"))

    lines[1] = "[" .. lines[1]
    lines[#lines] = lines[#lines] .. "][" .. label .. "]"

    return label
  end)

  insert_link_def(label, link)

  if link == "" then
    -- If link is empty place cursor at definition and start inserting
    set_cursor_at_last_content()
  else
    -- Set cursor at the end of `][]`.
    local end_row = selection[3] + 1
    -- End of selection + `][]
    local end_col = selection[4] + 3
    vim.api.nvim_win_set_cursor(0, { end_row, end_col })
  end
end

---@param opts { link_style?: "inline" | "collapsed_reference" | "full_reference" }
function M.create_link(opts)
  opts = vim.tbl_extend("force", { link_style = "inline" }, opts or {})

  local link = link_url_to_paste()

  -- Only paste in visual selection
  local mode = vim.api.nvim_get_mode().mode
  local is_visual = mode == "v" or mode == "V"
  if not is_visual then
    return
  end

  local selection = get_visual_range()

  if not ts.inside_block_element(selection) then
    vim.notify("Not completely inside a block element", vim.log.levels.DEBUG)
    return
  end

  if opts.link_style == "inline" then
    create_inline_link(link, selection)
  elseif opts.link_style == "collapsed_reference" then
    create_collapsed_reference_link(link, selection)
  elseif opts.link_style == "full_reference" then
    create_full_reference_link(link, selection)
  end
end

function M.select_link_url()
  local link = M.get_nearest_link()

  local row_start, col_start, row_end, col_end
  if link then
    row_start, col_start, row_end, col_end = get_link_destination_range(link)
  else
    local def = M.get_nearest_link_def()
    if def then
      row_start, col_start, row_end, col_end = get_link_destination_range(def)
    end
  end

  if not row_start then
    return
  end

  vim.api.nvim_buf_set_mark(0, "<", row_start + 1, col_start, {})
  vim.api.nvim_buf_set_mark(0, ">", row_end + 1, col_end - 1, {})
  vim.cmd("normal! gv")
end

---@param def TSNode
local function remove_link_def(def)
  local def_start, _, def_end, _ = vim.treesitter.get_node_range(def)
  vim.api.nvim_buf_set_lines(0, def_start, def_end, false, {})
end

function M.try_create_autolink()
  local url = vim.fn.expand('<cfile>')
  if not is_url(url) then
    return
  end

  local line = vim.api.nvim_get_current_line()
  local start = line:find(url)
  if not start then
    return
  end

  local curr_line = vim.api.nvim_win_get_cursor(0)[1] - 1

  vim.api.nvim_buf_set_text(0, curr_line, start - 1, curr_line, start + #url - 1, {
    "<" .. url .. ">"
  })
end

---@param opts? { reference_type?: "collapsed_reference" | "full_reference" }
function M.convert_link(opts)
  opts = vim.tbl_extend("force", { reference_type = "collapsed_reference" }, opts or {})

  local link = M.get_nearest_link()
  if not link then
    M.try_create_autolink()
    return
  end

  if link:type() == "autolink" then
    local destination = ts.get_text(link, 1, 1)
    local row_start, col_start, row_end, col_end = ts.get_range(link)

    vim.api.nvim_buf_set_text(0, row_start, col_start, row_end, col_end, {
      "[](" .. destination .. ")"
    })
    vim.api.nvim_win_set_cursor(0, { row_start + 1, col_start + 1 })
    vim.cmd("startinsert")
  elseif link:type() == "inline_link" then
    local destination = link:named_child(1)
    local row_start, col_start, row_end, col_end = ts.get_range(destination, 1, 1)
    local dest = vim.api.nvim_buf_get_text(0, row_start, col_start, row_end, col_end, {})[1]

    local link_text = ts.get_text(link:named_child(0), 1, 1)

    local label
    if opts.reference_type == "collapsed_reference" then
      label = link_text
      vim.api.nvim_buf_set_text(0, row_start, col_start - 1, row_end, col_end + 1, { "[]" })
    else
      label = slugify(link_text)
      vim.api.nvim_buf_set_text(0, row_start, col_start - 1, row_end, col_end + 1, {
        "[" .. label .. "]",
      })
    end

    insert_link_def(label, dest)
  else
    local label, label_text
    if link:type() == "collapsed_reference_link" then
      label = link:named_child(0)
      label_text = ts.get_text(label, 1, 1)
    else
      label = link:named_child(1)
      label_text = ts.get_text(label)
    end

    local def = find_link_def(label_text)
    if not def then
      vim.notify("No definition for label: " .. label_text, vim.log.levels.WARN)
      return
    end

    local url = ts.get_text(def:named_child(1))

    -- First remove the link reference definition.
    remove_link_def(def)

    -- Then convert to inline link.
    local row_start, col_start, row_end, col_end
    if link:type() == "collapsed_reference_link" then
      row_start, _, row_end, col_end = ts.get_range(link)
      col_start = col_end - 2
    else
      row_start, col_start, row_end, col_end = ts.get_range(label, -1, -1)
    end

    vim.api.nvim_buf_set_text(0, row_start, col_start, row_end, col_end, {
      "(" .. url .. ")",
    })
    vim.cmd("undojoin")
  end
end

return M
