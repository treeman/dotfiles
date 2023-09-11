-- Swapping parameters
require("iswap").setup({
	-- The keys that will be used as a selection, in order
	keys = "nialthrd",

	-- Grey out the rest of the text when making a selection
	-- grey = 'disable',

	-- Highlight group for the sniping value (asdf etc.)
	hl_snipe = "ErrorMsg",

	-- Highlight group for the visual selection of terms
	hl_selection = "CursorLine",

	-- Highlight group for the greyed background
	-- default 'Comment'
	--hl_grey = 'LineNr',

	-- Automatically swap with only two arguments
	autoswap = true,
})
