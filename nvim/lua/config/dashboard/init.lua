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
        hl_shortcut = { { "GruvboxOrange", 0, #sc }, { "Text", #sc + 1, 20 } }
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
    button("s", "Scratch", ":e ~/norg/scratch.norg<CR>"),
    button("f", "Find files", ":Telescope find_files<cr>"),
    button("o", "Recent files", ":Telescope oldfiles<cr>"),
    { type = "padding", val = 1 },
    button("c", "Config", ":lua require('telescope.builtin').find_files( { cwd = vim.fn.expand('~/.config/nvim') })<CR>"),
    button("?", "Cheatsheet", ":CheatsheetEdit<cr>"),
    { type = "padding", val = 1 },
    button("u", "Update all", ":UpdateAll<cr>"),
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
  "s<c>                       Leap",
  "cs<target><replacement>    Change delimiters",
  "ds<char>                   Delete delimiters",
  "ys<motion><char>           Add delimiters",
  "v oil-ssh://server         Edit files on remote",
}

local working_on_transformed = {}
for k,v in pairs(working_on) do
  local cmd_width = 25
  working_on_transformed[k] = {
      type = "text",
      val = string.format("%-50s", v),
      opts = {
        position = "center",
        hl = { { "Statement", 0, cmd_width }, { "GruvboxAqua", cmd_width + 1, 50 } }
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
    -- Could be more clever here I guess
    button("w", "Edit working on", ":e ~/.config/nvim/lua/config/dashboard/init.lua<CR>"),
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
