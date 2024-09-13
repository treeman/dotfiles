local map = vim.keymap.set

-- Keymaps that are sent to plugins during configuration
-- Return a module here so we can keep all keymap definitions
-- in the same place.
-- This is only for global keys, other contexts
-- (such as in the cmp selection menu) may be defined
-- in their respective files.
local M = {}

-- Wrap it in a function to prevent requiring this file evaluates
-- global keymaps multiple times.
M.init = function()
  local normal_keyboard = require("util.keyboard").has_normal_keyboard()

  -- Copy/paste to mouse clipboard quickly
  map("n", "<leader>p", '"*p', { silent = true, desc = "Paste from mouse" })
  map("n", "<leader>P", '"*P', { silent = true, desc = "Paste before from mouse" })
  map("n", "<leader>y", '"*y', { silent = true, desc = "Yank into mouse" })
  map("n", "<leader>Y", '"*Y', { silent = true, desc = "Yank line into mouse" })
  -- Don't overwrite register when pasting in visual selection
  map("x", "p", '"_dP')

  -- Use ( as [ everywhere for T-34 that has ()
  -- on the base layer, but [] are hidden.
  map("n", "(", "[", { remap = true })
  map("n", ")", "]", { remap = true })
  map("o", "(", "[", { remap = true })
  map("o", ")", "]", { remap = true })
  map("x", "(", "[", { remap = true })
  map("x", ")", "]", { remap = true })

  map("n", "]q", ":cnext<cr>", { desc = "Next quickfix" })
  map("n", "[q", ":cprevious<cr>", { desc = "Prev quickfix" })
  map("n", "]Q", ":clast<cr>", { desc = "Last quickfix" })
  map("n", "[Q", ":cfirst<cr>", { desc = "First quickfix" })

  map("n", "]l", ":lnext<cr>", { desc = "Next loclist" })
  map("n", "[l", ":lprevious<cr>", { desc = "Prev loclist" })
  map("n", "]L", ":llast<cr>", { desc = "Last loclist" })
  map("n", "[L", ":lfirst<cr>", { desc = "First loclist" })

  -- Happy windows switching
  if normal_keyboard then
    map("n", "<C-h>", "<c-w>h")
    map("n", "<C-j>", "<c-w>j")
    map("n", "<C-k>", "<c-w>k")
    map("n", "<C-l>", "<c-w>l")

    map("t", "<C-h>", "<c-w>h")
    map("t", "<C-j>", "<c-w>j")
    map("t", "<C-k>", "<c-w>k")
    map("t", "<C-l>", "<c-w>l")
  else
    map("n", "<C-left>", "<c-w>h")
    map("n", "<C-down>", "<c-w>j")
    map("n", "<C-up>", "<c-w>k")
    map("n", "<C-right>", "<c-w>l")

    map("t", "<C-left>", "<c-w>h")
    map("t", "<C-down>", "<c-w>j")
    map("t", "<C-up>", "<c-w>k")
    map("t", "<C-right>", "<c-w>l")
  end

  -- Don't get caught in terminal
  map("t", "<Esc>", "<C-\\><C-n>")

  -- Move visual lines
  map("n", "<up>", "gk")
  map("n", "<down>", "gj")

  -- Don't move cursor when joining lines
  map("n", "J", "mzJ`z")

  -- Maximize current buffer
  map("n", "<C-w>m", ":MaximizerToggle<CR>", { silent = true, desc = "Maximize window" })

  -- Edit file with pre-filled path from the current file
  map("n", "<leader>ef", ":e <C-R>=expand('%:p:h') . '/'<CR>", { desc = "Edit relative file" })

  -- Goto previous buffer
  map("n", "<leader>B", ":edit #<CR>", { desc = "Previous buffer" })

  -- Edit files in a buffer
  map("n", "<leader>ed", ":Oil .<CR>", { desc = "Edit workspace" })
  -- Edit files in within the current directory
  map("n", "<leader>eD", ":Oil <C-R>=expand('%:p:h')<CR><CR>", { desc = "Edit relative workspace" })

  map("n", "<leader>d", ":Neotree toggle=true<CR>", { desc = "Neotree" })
  map("n", "<leader>t", ":Trouble cascade toggle<cr>", { desc = "Diagnostics" })

  map("n", "<leader>es", ":e ~/org/scratch.dj<CR>", { desc = "Scratch" })
  map("n", "<leader>ej", ":e ~/org/journal.dj<CR>", { desc = "Journal" })
  map("n", "<leader>eg", ":e ~/org/goals.dj<CR>", { desc = "Goals" })
  map("n", "<leader>eh", ":e ~/org/habits.dj<CR>", { desc = "Habits" })

  -- Git
  -- map("n", "gs", ":Neogit<CR>", { desc = "Git status" })
  map("n", "<leader>g", ":Neogit<CR>", { desc = "Git status" })
  map("n", "gt", ":Tardis git<CR>", { desc = "Git timemachine (Tardis)" })
  map("n", "g<space>", ":Git ", { desc = "Git" })
  map("n", "gb", function()
    require("telescope.builtin").git_branches()
  end, { silent = true, desc = "Git branches" })
  map("n", "gB", ":BlameToggle<CR>", { silent = true, desc = "Git blame" })
  -- Jujutsu
  map("n", "<leader>j", ":JJ ", { desc = "Jujutsu" })
  map("n", "gl", function()
    require("jj.views.log").open()
  end, { desc = "Jujutsu log" })
  map("n", "gs", function()
    require("jj").execute("status")
  end, { desc = "Jujutsu status" })
  map("n", "gd", function()
    require("jj").execute("diff")
  end, { desc = "Jujutsu diff" })
  map("n", "gL", function()
    require("jj").execute("op log")
  end, { desc = "Jujutsu op log" })

  -- Blogging
  map("n", "gw", function()
    require("blog.telescope").find_markup()
  end, { desc = "Find blog draft" })

  map("n", "z=", function()
    require("telescope.builtin").spell_suggest()
  end, { silent = true, desc = "Spell suggest" })
  map("n", "<leader>f", function()
    require("telescope.builtin").find_files()
  end, { silent = true, desc = "Find files" })
  map("n", "<leader>F", function()
    require("telescope.builtin").find_files({ cwd = vim.fn.expand("%:p:h") })
  end, { silent = true, desc = "Find relative file" })

  map("n", "<leader>/", function()
    require("telescope.builtin").live_grep()
  end, { silent = true, desc = "Find in files" })
  map("n", "<leader>b", function()
    require("custom.telescope").open_buffer()
  end, { silent = true, desc = "Buffers" })
  map("n", "<leader>o", function()
    require("telescope.builtin").oldfiles()
  end, { silent = true, desc = "Old files" })

  --  Telescoping into a personal knowledge base is really pleasant,
  map("n", "<leader><leader>", function()
    require("config.org").open_org_file_telescope("")
  end, {
    desc = "Org",
  })
  map("n", "<leader>ep", function()
    require("config.org").open_org_file_telescope("projects")
  end, {
    desc = "Org projects",
  })
  map("n", "<leader>ea", function()
    require("config.org").open_org_file_telescope("areas")
  end, {
    desc = "Org areas",
  })
  map("n", "<leader>er", function()
    require("config.org").open_org_file_telescope("resources")
  end, {
    desc = "Org resources",
  })
  map("n", "<leader>eA", function()
    require("config.org").open_org_file_telescope("archive")
  end, {
    desc = "Org archive",
  })

  map("n", "<leader>hh", function()
    require("telescope.builtin").help_tags()
  end, { silent = true, desc = "Help tags" })

  -- Ideas
  --require('telescope.builtin').git_commits()
  --require('telescope.builtin').git_bcommits()
  --require('telescope.builtin').git_bcommits_range()
  map("n", "<leader>rq", function()
    require("replacer").run()
  end, {
    silent = true,
    desc = "Make quickfix editable for replacing in",
  })

  -- Replace word under cursor
  map("n", "<leader>rw", "<cmd>SearchReplaceSingleBufferCWord<cr>", {
    desc = "Replace CWord",
  })
  -- Replace WORD under cursor
  map("n", "<leader>rW", "<cmd>SearchReplaceSingleBufferCWORD<cr>", {
    desc = "Replace CWORD",
  })
  -- Replace "expression" (includes dots, not sure how useful this is)
  map("n", "<leader>re", "<cmd>SearchReplaceSingleBufferCExpr<cr>", {
    desc = "Replace CExpr",
  })
  -- Replace visual selection
  map("v", "<C-r>", "<CMD>SearchReplaceSingleBufferVisualSelection<CR>", {
    desc = "Replace selection",
  })

  map("n", "<C-a>", function()
    require("dial.map").manipulate("increment", "normal")
  end, {
    desc = "Increment number",
  })
  map("n", "<C-x>", function()
    require("dial.map").manipulate("decrement", "normal")
  end, {
    desc = "Decrement number",
  })
  map("v", "<C-a>", function()
    require("dial.map").manipulate("increment", "visual")
  end, {
    desc = "Increment number",
  })
  map("v", "<C-x>", function()
    require("dial.map").manipulate("decrement", "visual")
  end, {
    desc = "Decrement number",
  })

  map("n", "]t", function()
    require("trouble").next({ skip_groups = true, jump = true })
  end, {
    desc = "Next trouble",
    silent = true,
  })
  map("n", "[t", function()
    require("trouble").prev({ skip_groups = true, jump = true })
  end, {
    desc = "Prev trouble",
    silent = true,
  })
  map("n", "]T", function()
    require("trouble").last({ skip_groups = true, jump = true })
  end, {
    desc = "Last trouble",
    silent = true,
  })
  map("n", "[T", function()
    require("trouble").first({ skip_groups = true, jump = true })
  end, {
    desc = "First trouble",
    silent = true,
  })

  map("n", "<leader>u", function()
    require("undotree").toggle()
  end, {
    desc = "Undotree",
  })

  local old_gx = vim.fn.maparg("gx", "n", nil, true)
  map("n", "gx", require("custom.open").gx_extended(old_gx.callback), { desc = old_gx.desc })
end

M.buf_blog = function(buffer)
  map("n", "<localleader>d", function()
    require("blog.interaction").goto_def()
  end, { buffer = buffer, desc = "Goto definition" })
  map(
    "n",
    "<localleader>h",
    require("blog.interaction").hover,
    { buffer = buffer, desc = "Hover help" }
  )
end

M.djot = function()
  map(
    "n",
    "<localleader>w",
    ":Trouble ts_headings toggle<CR>",
    { buffer = 0, desc = "Display headings" }
  )
end

-- Maps four pairs:
-- [f, [F, ]f, ]F
-- for the given treesitter textobject
-- see: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
local ts_move_keys = {
  f = { query = "@function.outer", desc = "goto function" },
  a = { query = "@attribute.inner", desc = "goto attribute" },
  b = { query = "@block.inner", desc = "goto block" },
  c = { query = "@class.outer", desc = "goto class" },
  x = { query = "@comment.outer", desc = "goto comment" },
  g = { query = { "@class.outer", "@function.outer" }, desc = "goto major" },
  -- t = { query = "@heading1", desc = "goto heading1" },
}

M.ts_goto_next_start = {}
M.ts_goto_next_end = {}
M.ts_goto_previous_start = {}
M.ts_goto_previous_end = {}

for k, v in pairs(ts_move_keys) do
  M.ts_goto_next_start["]" .. k] = v
  M.ts_goto_next_end["]" .. string.upper(k)] = v
  M.ts_goto_previous_start["[" .. k] = v
  M.ts_goto_previous_end["[" .. string.upper(k)] = v
end

-- Some symbolic keymaps that don't have a string.upper()
M.ts_goto_next_start["]="] = { query = "@statement.outer", desc = "goto statement" }
M.ts_goto_previous_start["[="] = { query = "@statement.outer", desc = "goto statement" }
M.ts_goto_next_start["],"] = { query = "@parameter.outer", desc = "goto parameter" }
M.ts_goto_previous_start["[,"] = { query = "@parameter.outer", desc = "goto parameter" }

M.ts_swap_next = {
  ["<leader>s"] = { query = "@parameter.inner", desc = "Swap next parameter" },
}
M.ts_swap_previous = {
  ["<leader>S"] = { query = "@parameter.inner", desc = "Swap previous parameter" },
}

M.ts_select = {
  ["af"] = { query = "@function.outer", desc = "Select outer function" },
  ["if"] = { query = "@function.inner", desc = "Select inner function" },
  ["ac"] = { query = "@class.outer", desc = "Select outer class" },
  ["ic"] = { query = "@class.inner", desc = "Select inner class" },
  ["ab"] = { query = "@block.outer", desc = "Select outer block" },
  ["ib"] = { query = "@block.inner", desc = "Select inner block" },
  ["aa"] = { query = "@attribute.outer", desc = "Select outer attribute" },
  ["ia"] = { query = "@attribute.inner", desc = "Seect inner attribute" },
  ["ax"] = { query = "@comment.outer", desc = "Select outer comment" },
  ["ix"] = { query = "@comment.inner", desc = "Select inner comment" },
  ["a="] = { query = "@statement.outer", desc = "Select outer statement" },
  ["i="] = { query = "@statement.inner", desc = "Select inner statement" },
  ["a,"] = { query = "@parameter.outer", desc = "Select outer parameter" },
  ["i,"] = { query = "@parameter.inner", desc = "Select inner parameter" },
}

M.neotest = function(buffer)
  map("n", "<leader>x", function()
    require("neotest").run.run()
  end, { buffer = buffer, desc = "Neotest run test at cursor" })
  map("n", "<leader>X", function()
    require("neotest").run.run(vim.fn.expand("%"))
  end, { buffer = buffer, desc = "Neotest run tests in file" })
  map("n", "<leader>m", function()
    require("neotest").run.run(vim.loop.cwd())
  end, { buffer = buffer, desc = "Neotest run tests in workspace" })

  map("n", "<leader>n", function()
    require("neotest").output_panel.toggle()
  end, { buffer = buffer, desc = "Neotest toggle panel tab" })
  map("n", "<leader>N", function()
    require("neotest").summary.toggle()
  end, { buffer = buffer, desc = "Neotest toggle summary tab" })
end

M.buf_lsp = function(_, buffer)
  -- NOTE there are other cool possibilities listed in nvim-lspconfig
  map(
    "n",
    "<localleader>D",
    vim.lsp.buf.declaration,
    { silent = true, buffer = buffer, desc = "Declaration" }
  )
  map(
    "n",
    "<localleader>d",
    vim.lsp.buf.definition,
    { silent = true, buffer = buffer, desc = "Definition" }
  )
  map(
    "n",
    "<localleader>r",
    vim.lsp.buf.references,
    { silent = true, buffer = buffer, desc = "References" }
  )
  -- Jumping doesn't quite work, don't switch yet.
  -- map(
  -- 	"n",
  -- 	"<localleader>r",
  -- 	":TroubleToggle lsp_references<CR>",
  -- 	{ silent = true, buffer = buffer, desc = "References" }
  -- )
  map(
    "n",
    "<localleader>i",
    vim.lsp.buf.implementation,
    { silent = true, buffer = buffer, desc = "Implementation" }
  )
  map(
    "n",
    "<localleader>t",
    vim.lsp.buf.type_definition,
    { silent = true, buffer = buffer, desc = "Type definition" }
  )
  map("n", "<localleader>h", vim.lsp.buf.hover, { silent = true, buffer = buffer, desc = "Hover" })
  map(
    "n",
    "<localleader>s",
    vim.lsp.buf.signature_help,
    { silent = true, buffer = buffer, desc = "Signature help" }
  )
  map(
    "n",
    "<localleader>x",
    vim.lsp.buf.code_action,
    { silent = true, buffer = buffer, desc = "Code action" }
  )
  map("n", "<localleader>l", "<cmd>lua vim.diagnostic.open_float({ focusable = false })<CR>")
  map(
    "n",
    "<localleader>R",
    vim.lsp.buf.rename,
    { silent = true, buffer = buffer, desc = "Rename" }
  )
  map(
    "n",
    "<localleader>I",
    vim.lsp.buf.incoming_calls,
    { silent = true, buffer = buffer, desc = "Incoming calls" }
  )
  map(
    "n",
    "<localleader>O",
    vim.lsp.buf.outgoing_calls,
    { silent = true, buffer = buffer, desc = "Outgoing calls" }
  )
  map(
    "n",
    "<localleader>w",
    ":Trouble symbols toggle<CR>",
    { silent = true, buffer = buffer, desc = "Document symbols" }
  )
end

-- These are default bindings in Neovim but they don't open the diagnostic floats immediately.
-- Calling them manually does though...
M.global_lsp = function()
  map("n", "]d", vim.diagnostic.goto_next, { silent = true, desc = "Next diagnostic" })
  map("n", "[d", vim.diagnostic.goto_prev, { silent = true, desc = "Prev diagnostic" })
end

M.gitsigns = function(buffer)
  local gitsigns = package.loaded.gitsigns
  map("n", "]h", gitsigns.next_hunk, { silent = true, buffer = buffer, desc = "Next hunk" })
  map("n", "[h", gitsigns.prev_hunk, { silent = true, buffer = buffer, desc = "Prev hunk" })
  map(
    "n",
    "<leader>hs",
    gitsigns.stage_hunk,
    { silent = true, buffer = buffer, desc = "Stage hunk" }
  )
  map(
    "n",
    "<leader>hr",
    gitsigns.reset_hunk,
    { silent = true, buffer = buffer, desc = "Reset hunk" }
  )
  map("v", "<leader>hs", function()
    gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
  end, { silent = true, buffer = buffer, desc = "Stage hunk" })
  map("v", "<leader>hr", function()
    gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
  end, { silent = true, buffer = buffer, desc = "Reset hunk" })
  map("n", "<leader>hb", function()
    gitsigns.blame_line({ full = true })
  end, { silent = true, buffer = buffer, desc = "Blame hunk" })
end

M.marks = {
  set = "m",
  delete = "dm",
  delete_line = "dm-",
  delete_buf = "dm<space>",
  next = "]m",
  prev = "[m",
  preview = "m:",
}

M.pollen = function()
  map("i", "<C-l>", "λ")
  map("i", "<C-e>", "◊")
end

return M
