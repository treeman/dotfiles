local header = {
  type = "text",
  val = {
    [[                                  __]],
    [[     ___     ___    ___   __  __ /\_\    ___ ___]],
    [[    / _ `\  / __`\ / __`\/\ \/\ \\/\ \  / __` __`\]],
    [[   /\ \/\ \/\  __//\ \_\ \ \ \_/ |\ \ \/\ \/\ \/\ \]],
    [[   \ \_\ \_\ \____\ \____/\ \___/  \ \_\ \_\ \_\ \_\]],
    [[    \/_/\/_/\/____/\/___/  \/__/    \/_/\/_/\/_/\/_/]],
  },
  opts = {
    position = "center",
    hl = "Type",
    -- wrap = "overflow";
  },
}

local if_nil = vim.F.if_nil

local leader = "SPC"

--- @param sc string
--- @param txt string
--- @param keybind string? optional
--- @param keybind_opts table? optional
local function button(sc, txt, keybind, keybind_opts)
  local sc_ = sc:gsub("%s", ""):gsub(leader, "<leader>")

  local opts = {
    position = "center",
    shortcut = sc .. " ",
    cursor = 52,
    width = 50,
    align_shortcut = "left",
    hl_shortcut = { { "Function", 0, #sc }, { "Text", #sc + 1, 20 } },
  }
  if keybind then
    keybind_opts = if_nil(keybind_opts, { noremap = true, silent = true, nowait = true })
    opts.keymap = { "n", sc_, keybind, keybind_opts }
  end

  local function on_press()
    local key = vim.api.nvim_replace_termcodes(keybind or sc_ .. "<Ignore>", true, false, true)
    vim.api.nvim_feedkeys(key, "t", false)
  end

  return {
    type = "button",
    val = txt,
    on_press = on_press,
    opts = opts,
  }
end

local buttons = {
  type = "group",
  val = {
    -- button("s", "Scratch", ":e ~/org/scratch.dj<CR>"),
    -- button("g", "Goals", ":e ~/org/goals.dj<CR>"),
    -- button("h", "Habits", ":e ~/org/habits.dj<CR>"),
    -- { type = "padding", val = 1 },
    button(
      "c",
      "Config",
      ":lua require('telescope.builtin').find_files( { cwd = vim.fn.expand('~/.config/nvim') })<CR>"
    ),
    button("?", "Cheatsheet", ":CheatsheetEdit<cr>"),
    { type = "padding", val = 1 },
    button("u", "Update all", ":UpdateAll<cr>"),
    button("l", "Lazy", ":Lazy<cr>"),
    button("m", "Mason", ":Mason<cr>"),
  },
}

local working_on = {
  -- Meh, just manually align middle space. Like a grug.
  "<leader>w                  Word flash",
  -- "s<c>                       Leap",
  -- "yr<target>                 Remote yank",
  "cs<target><replacement>    Change delimiters",
  "ds<char>                   Delete delimiters",
  "ys<motion><char>           Add delimiters",
  -- "v oil-ssh://server         Edit files on remote",
}

local working_on_transformed = {}
for k, v in pairs(working_on) do
  local cmd_width = 25
  working_on_transformed[k] = {
    type = "text",
    val = string.format("%-50s", v),
    opts = {
      position = "center",
      hl = { { "DiagnosticError", 0, cmd_width }, { "@label", cmd_width + 1, 50 } },
    },
  }
end

local focus_commands = {
  type = "group",
  val = working_on_transformed,
}

local section = {
  header = header,
  buttons = buttons,
  focus_commands = focus_commands,
  buttons2 = button("w", "Edit working on", ":e ~/.config/nvim/lua/config/dashboard.lua<CR>"),
  fortune = {
    type = "text",
    val = "", -- Set during config as we can't require alpha in lazy opts
    opts = {
      position = "center",
      hl = "Comment",
    },
  },
  lazy = {
    type = "text",
    val = "", -- Set by autocommand created in lazy config
    opts = {
      position = "center",
      hl = "Ignore",
    },
  },
}

return {
  section = section,
  config = {
    layout = {
      { type = "padding", val = 1 },
      section.header,
      { type = "padding", val = 2 },
      section.buttons,
      { type = "padding", val = 2 },
      section.focus_commands,
      { type = "padding", val = 1 },
      -- Could be more clever here I guess
      section.buttons2,
      { type = "padding", val = 1 },
      section.fortune,
      { type = "padding", val = 2 },
      section.lazy,
    },
  },
}