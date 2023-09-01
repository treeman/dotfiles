local opts = {
  auto_update = true,
  run_on_start = true,
}

return {
  "WhoIsSethDaniel/mason-tool-installer.nvim",
  opts = opts,
  cmd = "MasonToolsUpdate",
  event = "BufReadPre",
  dependencies = "williamboman/mason.nvim",
}
