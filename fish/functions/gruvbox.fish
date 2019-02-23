#!/usr/bin/fish

function gruvbox --description 'Apply gruvbox theme'
    set bg0_hard        1d2021
    set bg0_medium      282828
    set bg0_soft        32302f
    set bg1             3c3836
    set bg2             504945
    set bg3             665c54
    set bg4             7c6f64

    set fg              ebdbb2
    set fg0             fbf1c7
    set fg1             ebdbb2
    set fg2             d5c4a1
    set fg3             bdae93
    set fg4             a89984

    set red             cc241d
    set green           98971a
    set yellow          d79921
    set blue            458588
    set purple          b16286
    set aqua            689d6a
    set orange          d65d0e

    set br_red          fb4934
    set br_green        b8bb26
    set br_yellow       fabd2f
    set br_blue         83a598
    set br_purple       d3869b
    set br_aqua         8ec07c
    set br_orange       d65d0e

    set gray_256        a89984
    set gray_255        928374

    # Set the colors I couldn't find a use for to "$br_red --underline"
    set fish_color_normal               $fg # default color
    set fish_color_command              $br_purple # the color for commands
    set fish_color_quote                $br_green # the color for quoted blocks of text
    set fish_color_redirection          $fg # the color for IO redirections
    set fish_color_end                  $gray_256 # the color for process separators like ';' and '&'
    set fish_color_error                $br_red # the color used to highlight potential errors
    set fish_color_param                $br_blue # the color for regular command parameters
    set fish_color_comment              $gray_255 --italics # the color used for code comments
    set fish_color_match                $br_red --underline # the color used to highlight matching parenthesis
    set fish_color_selection            $br_red --underline # the color used when selecting text (in vi visual mode)
    set fish_color_search_match         \x1e\x2d\x2dbackground\x3d$bg1 # used to highlight history search matches and the selected pager item (must be a background)
    set fish_color_operator             $blue # the color for parameter expansion operators like '*' and '~'
    set fish_color_escape               $yellow # the color used to highlight character escapes like '\n' and '\x70'
    set fish_color_cwd                  $green # the color used for the current working directory in the default prompt
    set fish_color_autosuggestion       $gray_256 # the color used for autosuggestions
    set fish_color_user                 $br_red --underline # the color used to print the current username in some of fish default prompts
    set fish_color_host                 $br_red --underline # the color used to print the current host system in some of fish default prompts
    set fish_color_cancel               $br_orange # the color for the '^C' indicator on a canceled command

    # Completion pager:
    set fish_pager_color_prefix         $br_aqua # the color of the prefix string, i.e. the string that is to be completed
    set fish_pager_color_completion     $red # the color of the completion itself
    set fish_pager_color_description    $fg3 # the color of the completion description
    set fish_pager_color_progress       $gray_256 # the color of the progress bar at the bottom left corner
    set fish_pager_color_secondary      $br_red --underline # the background color of the every second completion

    # NOTE
    # Tried to set background colors with "--background $bg1" but it generates:
    # SETUVAR fish_color_search_match:\x2d\x2dbackground\x1e3c3836
    # in fish_variables
    # instead of the correct
    # SETUVAR fish_color_search_match:\x1e\x2d\x2dbackground\x3d3c3836

    # Not sure of the color for commands?
end

