local opts = {
	-- space_char_blankline = " ",
	-- show_current_context = true,
	-- show_current_context_start = false,
	-- show_current_context_start_on_current_line = false,
	-- show_trailing_blankline_indent = false,
	-- show_first_indent_level = false,

	-- context_patterns = {
	--   "class",
	--   "^func",
	--   "method",
	--   "^if",
	--   "while",
	--   "for",
	--   "with",
	--   "try",
	--   "except",
	--   "arguments",
	--   "argument_list",
	--   "object",
	--   "dictionary",
	--   "element",
	--   "table",
	--   "tuple",
	--   "do_block",
	-- },
}
return {
	"lukas-reineke/indent-blankline.nvim",
	opts = opts,
	main = "ibl",
	event = { "BufReadPre", "BufNewFile" },
}
