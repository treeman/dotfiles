function! HandleURL()
  " FIXME doesn't handle surrounding " etc
  let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
  echo s:uri
  if s:uri != ""
    silent exec "!firefox '".s:uri."'"
  else
    echo "No URI found in line."
  endif
endfunction

