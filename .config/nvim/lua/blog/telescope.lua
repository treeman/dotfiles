local entry_display = require("telescope.pickers.entry_display")
local conf = require("telescope.config").values
local content = require("blog.content")
local finders = require("telescope.finders")
local pickers = require("telescope.pickers")
local previewers = require("telescope.previewers")
local sorters = require("telescope.sorters")
local telescope_utils = require("telescope.utils")

local M = {}

local function split_prompt(prompt)
  local tags = {}
  local series = {}
  local title = {}
  local path = {}
  for word in prompt:gmatch("([^%s]+)") do
    local fst = word:sub(1, 1)
    if fst == "@" then
      table.insert(tags, word:sub(2))
    elseif fst == "#" then
      table.insert(series, word:sub(2))
    elseif fst == "/" then
      table.insert(path, word)
    else
      table.insert(title, word)
    end
  end

  return {
    tags = tags,
    series = series,
    path = path,
    title = vim.fn.join(title, " "),
  }
end

-- Score a prompt element against an entry element using `sorter`.
--
-- Prompt elements should be a list of words from the prompt.
-- For example a prompt with tags like `@tag1 @tag2`
-- can produce the `{ "tag1", "tag2" }` `prompt_elements`.
-- Every prompted element needs a match, otherwise the entry will
-- get filtered.
--
-- The `entry_element` could either be a string (such a series id)
-- or a list of strings (multiple tags).
--
-- This function will clamp the score to 0..1, and return -1
-- if it should remove the entry (we prompted for it, but got no match).
-- In the case of multiple prompt elements, each individual element
-- is worth 1 / count prompt_elements.
local function score_element(prompt_elements, entry_element, sorter)
  if prompt_elements == nil then
    return 0
  end

  -- We didn't prompt for this type, ignore it.
  if next(prompt_elements) == nil then
    return 0
  end

  -- We prompted for this type, but entry didn't have it, so remove the entry.
  -- For example if we prompt for a series, this removes all posts
  -- without a series.
  if not entry_element then
    return -1
  end

  -- Convert multiple entry values to a string like `tag1:tag2`.
  local entry
  if type(entry_element) == "string" then
    entry = entry_element
  elseif type(entry_element) == "table" then
    entry = vim.fn.join(entry_element, ":")
  end

  local total = 0
  for _, prompt_element in ipairs(prompt_elements) do
    local score = sorter:scoring_function(prompt_element, entry)
    -- Require a match for every element.
    if score < 0 then
      return -1
    end
    total = total + score
  end

  -- Clamp score to max 1.
  return total / #prompt_elements
end

local function score_item_type(item)
  local function item_score()
    if item.type == "Post" then
      if item.is_draft then
        return 0
      else
        return 0.1
      end
    elseif item.type == "Project" then
      return 0.1
    elseif item.type == "Game" then
      return 0.1
    elseif item.type == "Standalone" then
      if item.is_draft then
        return 0.01
      else
        return 0.11
      end
    elseif item.type == "Series" then
      return 0.12
    elseif item.type == "Projects" then
      return 0.13
    else
      vim.notify("Unknown item type: " .. item.type, vim.log.levels.ERROR)
      return 0
    end
  end

  return item_score()
end

local function score_date(entry)
  local entry_date
  if entry.created then
    -- Remove `-` from entry date, so it's like a number.
    entry_date = string.gsub(entry.created, "-", "")
  elseif entry.published then
    -- Remove `-` from entry date, so it's like a number.
    entry_date = string.gsub(entry.published, "-", "")
  elseif entry.year then
    entry_date = tostring(entry.year) .. "0000"
  end

  if not entry_date then
    return 1
  end

  -- Date of my first blog post as a number.
  local beginning_of_time = 20090621
  -- Today's date as a number.
  local today = os.date("%Y%m%d")
  -- Place the number on a 0..1 scale, where 1 is today (`1 -` reverses, otherwise 1
  -- would be the beginning of time).
  return 1 - (entry_date - beginning_of_time) / (today - beginning_of_time)
end

local function content_sorter(opts)
  opts = opts or {}
  local fzy_sorter = sorters.get_fzy_sorter(opts)

  return sorters.Sorter:new({
    discard = true,

    scoring_function = function(_, prompt, entry)
      prompt = split_prompt(prompt)

      -- Score and filter against series, tags, and title separately.
      -- If any element returns a 0, it means nothing matched but we shouldn't filter.
      -- If it returns < 0, it means it did not match and should remove the entry.
      local series_score = score_element(prompt.series, entry.series, fzy_sorter)
      if entry.series then
        series_score = score_element(prompt.series, entry.series, fzy_sorter)
      elseif entry.type == "Series" then
        series_score = score_element(prompt.series, entry.id, fzy_sorter)
      end
      if series_score < 0 then
        return -1
      end

      local tags_score = score_element(prompt.tags, entry.tags, fzy_sorter)
      if tags_score < 0 then
        return -1
      end

      local path_score = score_element(prompt.path, entry.path, fzy_sorter)
      if path_score < 0 then
        return -1
      end

      local title_score = fzy_sorter:scoring_function(prompt.title, entry.title)
      if title_score < 0 then
        return -1
      end

      local type_score = score_item_type(entry)
      local date_score = score_date(entry)

      -- Date sorting is only worth 1/10 of the fuzzy scores.
      -- Why? I dunno, it felt like 1 was too much and 1/10 felt good.
      return series_score + tags_score + title_score + date_score / 10 + type_score
    end,
  })
end

local function make_series_display(item)
  local icon = "󰉋"
  local displayer = entry_display.create({
    separator = " ",
    items = {
      { width = 2 },
      { width = string.len(item.title) },
      { remaining = true },
    },
  })

  return displayer({
    { icon, "TelescopeResultsComment" },
    item.title,
    { item.id, "TelescopeResultsOperator" },
  })
end

local function make_post_display(item)
  local icon
  local icon_color = "TelescopeResultsComment"
  local ext = telescope_utils.file_extension(item.path)
  if item.is_draft then
    icon = "󰽉"
    icon_color = "Function"
  elseif ext == "dj" then
    icon = "󰛓"
  else
    icon = ""
  end

  local tags
  if type(item.tags) == "string" then
    tags = item.tags
  elseif type(item.tags) == "table" then
    tags = vim.fn.join(item.tags, ", ")
  end

  local series = item.series or ""

  local displayer = entry_display.create({
    separator = " ",
    items = {
      { width = 1 },
      { width = string.len(item.title) },
      { width = string.len(item.created) },
      { width = string.len(tags) },
      { remaining = true },
    },
  })

  return displayer({
    { icon, icon_color },
    item.title,
    { item.created, "TelescopeResultsComment" },
    { tags, "TelescopeResultsConstant" },
    { series, "TelescopeResultsOperator" },
  })
end

local function make_projects_display(item)
  local icon = "󰾁"

  local displayer = entry_display.create({
    separator = " ",
    items = {
      { width = 2 },
      { remaining = true },
    },
  })

  return displayer({
    { icon, "TelescopeResultsComment" },
    item.title,
  })
end

local function make_project_display(item)
  local icon = "󰙨"

  local displayer = entry_display.create({
    separator = " ",
    items = {
      { width = 1 },
      { width = string.len(item.title) },
      { remaining = true },
    },
  })

  return displayer({
    { icon, "TelescopeResultsComment" },
    item.title,
    { tostring(item.year), "TelescopeResultsComment" },
  })
end

local function make_game_display(item)
  local icon = ""

  local displayer = entry_display.create({
    separator = " ",
    items = {
      { width = 2 },
      { width = string.len(item.title) },
      { width = string.len(item.published) },
      { remaining = true },
    },
  })

  return displayer({
    { icon, "TelescopeResultsComment" },
    item.title,
    { item.published, "TelescopeResultsComment" },
    { item.event, "Character" },
  })
end

local function make_standalone_display(item)
  local icon = "󰾁"
  local icon_color = "TelescopeResultsComment"

  if item.is_draft then
    icon_color = "Function"
  end

  local displayer = entry_display.create({
    separator = " ",
    items = {
      { width = 2 },
      { remaining = true },
    },
  })

  return displayer({
    { icon, icon_color },
    item.title,
  })
end

local function make_display(entry)
  local item = entry.value
  if item.type == "Series" then
    return make_series_display(item)
  elseif item.type == "Post" then
    return make_post_display(item)
  elseif item.type == "Projects" then
    return make_projects_display(item)
  elseif item.type == "Game" then
    return make_game_display(item)
  elseif item.type == "Project" then
    return make_project_display(item)
  elseif item.type == "Standalone" then
    return make_standalone_display(item)
  else
    vim.notify("Unknown item type: " .. item.type, vim.log.levels.ERROR)
  end
end

-- New world order!
M.find_markup = function(opts)
  opts = opts or {}

  content.list_markup_content(function(files)
    pickers
      .new(opts, {
        finder = finders.new_table({
          results = files,
          entry_maker = function(entry)
            return {
              display = make_display,
              ordinal = entry,
              value = entry,
              -- Make standard action open `path` on <CR>
              filename = entry.path,
            }
          end,
        }),
        sorter = content_sorter(opts),
        previewer = previewers.new_buffer_previewer({
          title = "Content Preview",
          define_preview = function(self, entry)
            conf.buffer_previewer_maker(entry.value.path, self.state.bufnr, {
              bufname = self.state.bufname,
              winid = self.state.winid,
              preview = opts.preview,
              file_encoding = opts.file_encoding,
            })
          end,
        }),
      })
      :find()
  end)
end

return M
