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
    -- local sc_ = sc:gsub("%s", ""):gsub(leader, "<leader>")

    -- local opts = {
    --     position = "center",
    --     shortcut = "[" .. sc .. "] ",
    --     cursor = 1,
    --     -- width = 50,
    --     align_shortcut = "left",
    --     hl_shortcut = { { "Operator", 0, 1 }, { "Number", 1, #sc + 1 }, { "Operator", #sc + 1, #sc + 2 } },
    --     shrink_margin = false,
    -- }
    -- if keybind then
    --     keybind_opts = if_nil(keybind_opts, { noremap = true, silent = true, nowait = true })
    --     opts.keymap = { "n", sc_, keybind, keybind_opts }
    -- end

    -- local function on_press()
    --     local key = vim.api.nvim_replace_termcodes(keybind .. "<Ignore>", true, false, true)
    --     vim.api.nvim_feedkeys(key, "t", false)
    -- end

    -- return {
    --     type = "button",
    --     val = txt,
    --     on_press = on_press,
    --     opts = opts,
    -- }
    local sc_ = sc:gsub("%s", ""):gsub(leader, "<leader>")

    local opts = {
        position = "center",
        shortcut = sc .. " ",
        -- shortcut = "[" .. sc .. "] ",
        cursor = -1,
        width = 50,
        align_shortcut = "left",
        -- hl_shortcut = { { "Keyword", 0, #sc } },
        -- hl_shortcut = "Keyword",
        hl_shortcut = { { "GruvboxOrange", 0, #sc }, { "Text", #sc + 1, 20 } }
        -- hl_shortcut = { { "Text", 0, 1 }, { "GruvboxPurple", 1, #sc + 1 }, { "Text", #sc + 1, #sc + 2 } },
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
    button("f", "Find files", ":Telescope find_files<cr>"),
    button("o", "Recent files", ":Telescope oldfiles<cr>"),
    { type = "padding", val = 1 },
    button("c", "Config", ":lua require('telescope.builtin').find_files( { cwd = vim.fn.expand('~/.config/nvim') })<CR>"),
    button("?", "Cheatsheet", ":CheatsheetEdit<cr>"),
    { type = "padding", val = 1 },
    button("u", "Lazy Sync / TSUpdate / MasonToolsUpdate", ":UpdateAndSyncAll<cr>"),
    button("l", "Lazy", ":Lazy<cr>"),
    button("m", "Mason", ":Mason<cr>"),
    { type = "padding", val = 1 },
    button("n", "Norg", ":lua require('config.telescope_actions').open_norg('')<CR>"),
    button("p", "Projects", ":lua require('config.telescope_actions').open_norg('projects')<CR>"),
    button("a", "Areas", ":lua require('config.telescope_actions').open_norg('areas')<CR>"),
    button("r", "Resources", ":lua require('config.telescope_actions').open_norg('resources')<CR>"),
    button("A", "Archive", ":lua require('config.telescope_actions').open_norg('archive')<CR>"),
  }
}

local working_on = {
  -- Meh, just manually align middle space. Like a grug.
  "s<c>         Leap",
  "cs<c><c>     Change delimiters",
  "ds<c>        Delete delimiters",
  "ys<c><c>     Add delimiters",
}

local working_on_transformed = {}
for k,v in pairs(working_on) do
  working_on_transformed[k] = {
      type = "text",
      val = string.format("%-50s", v),
      opts = {
        position = "center",
        hl = { { "Statement", 0, 12 }, { "GruvboxAqua", 13, 50 } }
      }
  }
end

local focus_commands = {
  type = "group",
  val = working_on_transformed
}

return {
  layout = {
    { type = "padding", val = 1 },
    header,
    { type = "padding", val = 2 },
    buttons,
    { type = "padding", val = 2 },
    focus_commands,
    { type = "padding", val = 1 },
    {
      type = "text",
      val = require('alpha.fortune')(60),
      opts = {
        position = "center",
        hl = "Text",
      }
    }
  }
}
