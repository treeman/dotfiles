require("rest-nvim").setup({
  encode_url = false,
  result = {
    split = {
      horizontal = true,
      in_place = false,
      stay_in_current_window_after_split = true,
    },
  },
  highliht = {
    timeout = 200,
  },
})
