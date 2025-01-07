#!/usr/bin/fish

function melange --description 'Apply melange theme'
    echo "set melange colorscheme"

    set a_bg        292522
    set a_float     34302C
    set a_sel       403A36
    set a_ui        867462
    set a_com       C1A78E
    set a_fg        ECE1D7

    set b_red       D47766
    set b_yellow    EBC06D
    set b_green     85B695
    set b_cyan      89B3B6
    set b_blue      A3A9CE
    set b_magenta   CF9BC2

    set c_red       BD8183
    set c_yellow    E49B5D
    set c_green     78997A
    set c_cyan      7B9695
    set c_blue      7F91B2
    set c_magenta   B380B0

    set d_red       7D2A2F
    set d_yellow    8B7449
    set d_green     233524
    set d_cyan      253333
    set d_blue      273142
    set d_magenta   422741

    set fish_color_normal               $a_fg # default color
    set fish_color_command              $b_yellow # the color for commands
    set fish_color_quote                $b_blue --italics # the color for quoted blocks of text
    set fish_color_redirection          $b_red # the color for IO redirections
    set fish_color_end                  $c_yellow # the color for process separators like ';' and '&'
    set fish_color_error                $c_red # the color used to highlight potential errors
    set fish_color_param                $a_fg # the color for regular command parameters
    set fish_color_comment              $a_com --italics # the color used for code comments
    set fish_color_match                $a_ui # the color used to highlight matching parenthesis
    set fish_color_selection            $a_sel # the color used when selecting text (in vi visual mode)
    set fish_color_search_match         \x1e\x2d\x2dbackground\x3d$a_float # used to highlight history search matches and the selected pager item (must be a background)
    set fish_color_operator             $c_yellow # the color for parameter expansion operators like '*' and '~'
    set fish_color_escape               $b_blue # the color used to highlight character escapes like '\n' and '\x70'
    set fish_color_cwd                  $c_green # the color used for the current working directory in the default prompt
    set fish_color_autosuggestion       $a_ui # the color used for autosuggestions
    set fish_color_user                 $a_com # the color used to print the current username in some of fish default prompts
    set fish_color_host                 $a_com # the color used to print the current host system in some of fish default prompts
    set fish_color_cancel               $c_yellow # the color for the '^C' indicator on a canceled command

    # # Completion pager:
    set fish_pager_color_prefix         $b_yellow # the color of the prefix string, i.e. the string that is to be completed
    set fish_pager_color_completion     $b_magenta # the color of the completion itself
    set fish_pager_color_description    $a_com # the color of the completion description
    set fish_pager_color_progress       $a_ui # the color of the progress bar at the bottom left corner
    set fish_pager_color_secondary      $a_com # the background color of the every second completion
end
