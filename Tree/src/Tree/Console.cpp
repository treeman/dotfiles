#include <boost/weak_ptr.hpp>

#include "Tree/Console.hpp"
#include "Tree/Butler.hpp"
#include "Tree/Util.hpp"
#include "Tree/Log.hpp"

using Tree::Console;

Console::Console()
{
    Tree::GetSettings()->AddListener( this );

    render_str.SetFont( *Tree::GetButler()->GetFont(
        "fnt/arial.ttf", 10 ) );
    render_str.SetSize( 10 );

    x = 10; y = 10;
    w = 400;
    h = 300;

    cmd_pos = -1;
    input_line_pos = 0;

    sugg_pos = -1;

    text_off_left = 5;
    text_off_down = 5;
    text_off_top = 5;

    render_str.SetText( "tesT" );
    line_height = render_str.GetRect().GetHeight();

    opacity = 0xDD;
    fnt_opacity = 0xFF;
    blinkie_opacity = 0x55;
    selection_opacity = 0x66;
    delimiter_opacity = 0x22;

    fnt_color = sf::Color( 69, 221, 87 );
    fnt_history_color = sf::Color( 69, 221, 87, fnt_opacity );
    fnt_history_line_sign_color = sf::Color( 26, 161, 42, fnt_opacity );
    fnt_highlight_color = sf::Color( 235, 242, 48, fnt_opacity );
    fnt_suggestion_color = sf::Color( 69, 221, 87, fnt_opacity );
    blinkie_color = sf::Color( 122, 122, 122, blinkie_opacity );
    selection_color = sf::Color( 122, 122, 122, selection_opacity );

    large_back_color = sf::Color( 51, 51, 51, opacity );
    suggestion_back_color = sf::Color( 92, 92, 92, opacity );
    delimiter_color = sf::Color( 44, 44, 44, delimiter_opacity );

    back = sf::Shape::Rectangle( 0, 0, w, h, large_back_color );
    back.SetPosition( x, y );

    blinkie_time = 0.5;

    suggestion_box_width = 0;

    is_active = false;

    SelectionClear();
    blink_timer.Start();

    showDebug.reset( new Dator<bool>( false ) );
    Tree::GetSettings()->RegisterVariable( "console_show_debug", boost::weak_ptr<BaseDator>( showDebug ) );

    clearHistory.reset( new CallDator( boost::bind( &Console::Clear, this ) ) );
    Tree::GetSettings()->RegisterVariable( "console_clear", boost::weak_ptr<BaseDator>( clearHistory ) );

    showCommands.reset( new CallDator( boost::bind( &Console::ShowCommands, this ) ) );
    Tree::GetSettings()->RegisterVariable( "console_show_commands", boost::weak_ptr<BaseDator>( showCommands ) );

    showCommandsValues.reset( new CallDator( boost::bind( &Console::ShowCommandsValues, this ) ) );
    Tree::GetSettings()->RegisterVariable( "console_show_commands_values", boost::weak_ptr<BaseDator>( showCommandsValues ) );
}
Console::~Console()
{

}

bool Console::HandleEvent( sf::Event &e )
{
    if( !is_active ) {
        if( e.Type == sf::Event::KeyPressed && e.Key.Code == sf::Key::F1 ) {
            Activate();
        }
        else {
            return true;
        }
    }
    else if( e.Type == sf::Event::KeyPressed && e.Key.Code == sf::Key::F1 ) {
        Deactivate();
        return false;
    }

    if( e.Type == sf::Event::KeyPressed )
    {
        if( e.Key.Code == sf::Key::Return && input_line.size() > 0 ) {
            InputLineExecute();
        }
        else if( e.Key.Code == sf::Key::Space ) {
            SelectionDelete();
            InputLineAddChar( ' ', input_line_pos );
            InputLineMoveRight();
        }
        else if( e.Key.Code == sf::Key::Back ) {
            if( SelectionIsActive() ) {
                SelectionDelete();
            }
            else {
                InputLineDeleteChar( input_line_pos - 1 );
                InputLineMoveLeft();
            }
        }
        else if( e.Key.Code == sf::Key::A && e.Key.Control ) {
            SelectionAll();
        }
        else if( e.Key.Code == sf::Key::C && e.Key.Control ) {
            SelectionCopy();
        }
        else if( e.Key.Code == sf::Key::V && e.Key.Control ) {
            SelectionPaste( input_line_pos );
        }
        else if( e.Key.Code == sf::Key::Delete ) {
            if( SelectionIsActive() ) {
                SelectionDelete();
            }
            else {
                InputLineDeleteChar( input_line_pos );
            }
        }
        else if( e.Key.Code == sf::Key::Up ) {
            SelectionClear();
            if( IsSuggestionLocked() ) {
                MoveUpInSuggestion();
            }
            else {
                MoveBackInHistory();
            }
        }
        else if( e.Key.Code == sf::Key::Down ) {
            SelectionClear();
            if( IsHistoryLocked() ) {
                MoveForwardInHistory();
            }
            else {
                MoveDownInSuggestion();
            }
        }
        else if( e.Key.Code == sf::Key::Tab ) {
            if( IsSuggestionLocked() ) {
                AutoCompleteSuggestion();
            }
        }
        else if( e.Key.Code == sf::Key::Left ) {
            if( e.Key.Shift ) {
                SelectionMoveLeft();
            }
            else {
                SelectionClear();
            }
            InputLineMoveLeft();
        }
        else if( e.Key.Code == sf::Key::Right ) {
            if( e.Key.Shift ) {
                SelectionMoveRight();
            }
            else {
                SelectionClear();
            }
            InputLineMoveRight();
        }

        blink_timer.Restart();
    }
    else if( e.Type == sf::Event::TextEntered && e.Text.Unicode > 32 ) {
        SelectionDelete();
        InputLineAddChar( e.Text.Unicode, input_line_pos );
        InputLineMoveRight();

        blink_timer.Restart();
    }

    return false;
}

void Console::HearSetting( std::string setting, std::string value, std::string return_val )
{
    if( return_val.size() > 0 ) {
        AddHistory( return_val );
    }
}
std::string Console::Clear()
{
    big_history.clear();

    return "";
}
std::string Console::ShowCommands()
{
    StrList cmd_list = Tree::GetSettings()->GetSettings();
    for( StrList::iterator it = cmd_list.begin(); it != cmd_list.end(); ++it )
    {
        PushHistory( "* " + *it );
    }
    return "";
}

std::string Console::ShowCommandsValues()
{
    StrMap cmd_map = Tree::GetSettings()->GetSettingsValues();
    for( StrMap::iterator it = cmd_map.begin(); it != cmd_map.end(); ++it )
    {
        if( !it->second.empty() ) {
            PushHistory( "* " + it->first + ' ' + it->second );
        }
    }
    return "";
}

void Console::Update( float dt )
{
    if( !is_active ) {
        return;
    }
}
void Console::Render()
{
    if( !is_active ) {
        return;
    }

    RenderBones();
    RenderHistory();
    RenderInputLine();
    RenderTypeSuggestions();
    RenderDebug();
}

void Console::Activate()
{
    is_active = true;
}
void Console::Deactivate()
{
    is_active = false;
}

float Console::GetStringWidth( std::string str )
{
    render_str.SetText( str );
    return render_str.GetRect().GetWidth();
}

bool Console::IsHistoryLocked()
{
    return cmd_pos != -1;
}
void Console::MoveBackInHistory()
{
    if( !IsHistoryLocked() ) {
        InputLineSetBuff();
    }
    if( cmd_pos + 1 < (int)cmd_history.size() ) {
        cmd_pos++;
        InputLineSet( cmd_history.at( cmd_pos ) );
    }
}
void Console::MoveForwardInHistory()
{
    if( cmd_pos > -1 ) {
        cmd_pos--;
        if( cmd_pos != -1 ) {
            InputLineSet( cmd_history.at( cmd_pos ) );
        }
        else {
            InputLineRestoreBuff();
        }
    }
}
void Console::ResetHistoryPos()
{
    cmd_pos = -1;
}
void Console::AddHistory( std::string str )
{
    PushHistory( ">> " + str );
}
void Console::PushCmd( std::string str )
{
    AddHistory( str );
}
void Console::PushHistory( std::string str )
{
    if( GetStringWidth( str ) > ( w - 2 * text_off_left ) ) {
        //find the largest string that isn't too large
        std::string s; int i = 0;
        do {
            s += str[i];
            i++;
        }
        while( GetStringWidth( str ) < ( w - 2 * text_off_left )
            && i < (int)str.size() );

        //try to split on the nearest space of the new string instead
        int space_pos = s.rfind( ' ' );
        if( space_pos != -1 ) {
            PushHistory( str.substr( 0, space_pos ) );
            PushHistory( str.substr( space_pos + 1 ) );
        }
        //else just split and add
        else {
            PushHistory( str.substr( 0, i ) );
            PushHistory( str.substr( i ) );
        }

    }
    else {
        big_history.insert( big_history.begin(), str );
    }
}

bool Console::IsSuggestionLocked()
{
    return sugg_pos != -1;
}
void Console::MoveUpInSuggestion()
{
    if( sugg_pos > -1 ) {
        sugg_pos--;
        if( sugg_pos != -1 ) {
            StrMap::iterator it = suggestion_map.begin();
            std::advance( it, sugg_pos );

            InputLineSet( it->first + ' ', false );
        }
        else {
            InputLineRestoreBuff();
        }
    }
}
void Console::MoveDownInSuggestion()
{
    if( !IsSuggestionLocked() ) {
        InputLineSetBuff();
    }

    if( sugg_pos + 1 < (int)suggestion_map.size() ) {
        sugg_pos++;
        StrMap::iterator it = suggestion_map.begin();
        std::advance( it, sugg_pos );

        InputLineSet( it->first + ' ', false );
    }
}
void Console::AutoCompleteSuggestion()
{
    StrMap::iterator it = suggestion_map.begin();
    std::advance( it, sugg_pos );

    InputLineSet( it->first + ' ' + it->second );
}
void Console::UpdateSuggestionList()
{
    suggestion_map.clear();
    suggestion_box_width = 0;

    if( input_line.size() == 0 ) {
        return;
    }

    StrMap allsettings_map = Tree::GetSettings()->GetSettingsValues();
    for( StrMap::iterator it = allsettings_map.begin(); it != allsettings_map.end(); ++it )
    {
        std::string composit = it->first + ' ' + it->second;

        if( composit.find( input_line ) == 0 ) {
            suggestion_map.insert( *it );
            float w = GetStringWidth( composit );
            if( w > suggestion_box_width ) {
                suggestion_box_width = w;
            }
        }
    }

//  if( !suggestion_map.empty() && !IsSuggestionLocked() ) {
//      MoveDownInSuggestion();
//  }
}
void Console::ResetSuggestionPos()
{
    sugg_pos = -1;
}
void Console::InputLineSet( std::string str, bool update_suggestion )
{
    input_line = str;
    input_line_pos = str.size();
    if( update_suggestion ) {
        UpdateSuggestionList();
    }
}
void Console::InputLineOnInput()
{
    ResetHistoryPos();
    ResetSuggestionPos();
    UpdateSuggestionList();
    SelectionDelete();
}
void Console::InputLineSetBuff()
{
    input_line_buff = input_line;
}
void Console::InputLineRestoreBuff()
{
    InputLineSet( input_line_buff );
}
void Console::InputLineInsert( std::string str, int pos )
{
    input_line.insert( pos, str );
    input_line_pos += str.size();
    InputLineOnInput();
}
void Console::InputLineDeleteChar( int pos )
{
    if( pos < input_line_pos && pos >= 0 ) {
        input_line.erase( pos, 1 );
        InputLineOnInput();
    }
    else if( pos >= input_line_pos && pos != (int)input_line.size() ) {
        input_line.erase( pos, 1 );
        InputLineOnInput();
    }
}
void Console::InputLineAddChar( char ch, int pos )
{
    if( GetStringWidth( std::string( input_line + ch ) )
        < ( w - 2 * text_off_left ) )
    {
        input_line.insert( pos, 1, ch );
        InputLineOnInput();
    }
}
void Console::InputLineExecute()
{
    PushCmd( input_line );
    Tree::GetSettings()->ParseSetting( input_line );
    InputLineClear();
}
void Console::InputLineClear()
{
    input_line.clear();
    input_line_pos = 0;
    InputLineOnInput();
}
void Console::InputLineMoveLeft()
{
    if( input_line_pos > 0 ) {
        input_line_pos--;
    }
}
void Console::InputLineMoveRight()
{
    if( input_line_pos < (int)input_line.size() ) {
        input_line_pos++;
    }
}
bool Console::SelectionIsActive()
{
    return sel_start != -1 || sel_length != 0;
}
void Console::SelectionAll()
{
    sel_start = 0;
    sel_length = input_line.size();
}
void Console::SelectionMoveLeft()
{
    if( sel_start == -1 ) {
        sel_start = input_line_pos;
    }

    if( sel_start + sel_length > 0 ) {
        --sel_length;
    }
}
void Console::SelectionMoveRight()
{
    if( sel_start == -1 ) {
        sel_start = input_line_pos;
    }

    if( sel_start + sel_length < (int)input_line.size() ) {
        ++sel_length;
    }
}
void Console::SelectionClear()
{
    sel_length = 0;
    sel_start = -1;
}
void Console::SelectionDelete()
{
    if( sel_start != -1 || sel_length != 0 ) {

        std::string before, after;
        if( sel_length > 0 ) {
            before = input_line.substr( 0, sel_start );
            after = input_line.substr( sel_start + sel_length );
            input_line_pos = sel_start;
        }
        else {
            before = input_line.substr( 0, sel_start + sel_length );
            after = input_line.substr( sel_start );
            input_line_pos = sel_start + sel_length;
        }
        InputLineSet( before + after );
    }
    SelectionClear();
}

void Console::SelectionCopy()
{
    if( sel_length < 0 ) {
        selection_copy = input_line.substr( sel_start + sel_length, -sel_length );
    }
    else if( sel_length > 0 ) {
        selection_copy = input_line.substr( sel_start, sel_length );
    }
}
void Console::SelectionPaste( int pos )
{
    if( selection_copy.size() > 0 ) {
        InputLineInsert( selection_copy, pos );
    }
}

void Console::RenderBones()
{
    Tree::Draw( back );

//  //line above input line
//  const float x1 = x;
//  const float x2 = x + w;
//  const float y1 = y + h - text_off_down - fnt->GetHeight();
//
//  hge->Gfx_RenderLine( x1, y1, x2, y1, SETA( delimiter_color, delimiter_opacity ) );

//  const float x3 = x + w;
//  const float y2 = y + h;
//  const float y3 = y2 - text_off_down - fnt->GetHeight();
//
//  hge->Gfx_RenderLine( x3, y2, x3, y3, SETA( delimiter_color, delimiter_opacity ) );
}
void Console::RenderHistory()
{
    render_str.SetColor( fnt_history_color );

    const int max_n = (int)(( h - text_off_top - text_off_down ) / line_height - 2);
    int n = (int)big_history.size() - 1 < max_n ? big_history.size() - 1 : max_n;
    for( StrList::iterator it = big_history.begin(); it != big_history.end(); ++it )
    {
        if( it->substr( 0, 3 ) == ">> " )
        {
            render_str.SetColor( fnt_history_line_sign_color );
            render_str.SetText( ">> " );
            render_str.SetPosition( x + text_off_left,
                y + text_off_top + n * line_height );

            Tree::Draw( render_str );

            float pre_w = render_str.GetRect().GetWidth();

            render_str.SetColor( fnt_history_color );
            render_str.SetText( it->substr( 3 ) );
            render_str.SetPosition( x + text_off_left + pre_w,
                y + text_off_top + n * line_height );

            Tree::Draw( render_str );
        }
        else {
            render_str.SetText( it->c_str() );
            render_str.SetPosition( x + text_off_left,
                y + text_off_top + n * line_height );

            Tree::Draw( render_str );
        }
        --n;
        if( n < 0 ) {
            break;
        }
    }
}

void Console::RenderInputLine()
{
    render_str.SetPosition( x + text_off_left, y + h - text_off_down - line_height );
    render_str.SetText( input_line );

    Tree::Draw( render_str );

    if( SelectionIsActive() ) {

        std::string before, sel;
        if( sel_length > 0 ) {
            before = input_line.substr( 0, sel_start );
            sel = input_line.substr( sel_start, sel_length );
        }
        else {
            before = input_line.substr( 0, sel_start + sel_length );
            sel = input_line.substr( sel_start + sel_length, -sel_length );
        }

        float before_w = GetStringWidth( before );
        float sel_w = GetStringWidth( sel );

        const float sel_x1 = x + text_off_left + before_w;
        const float sel_x2 = sel_x1 + sel_w;
        const float sel_y1 = y + h - text_off_down;
        const float sel_y2 = y + h - text_off_down - line_height;

        sf::Shape rect = sf::Shape::Rectangle( sel_x1, sel_y1, sel_x2, sel_y2,
            selection_color );

        Tree::Draw( rect );
    }

    render_str.SetPosition( x + text_off_left, y + h - text_off_down - line_height );
    render_str.SetText( input_line );

    Tree::Draw( render_str );

    if( blink_timer.GetTime() < blinkie_time ) {

        //the little blinking helper thing
        float line_x = x + text_off_left +
            GetStringWidth( input_line.substr( 0, input_line_pos ) );
        float line_h = line_height;
        float line_y = y + h - text_off_down - line_height;

        sf::Shape line = sf::Shape::Line( line_x, line_y, line_x, line_y + line_h,
            1, blinkie_color );

        Tree::Draw( line );
    }
    else if( blink_timer.GetTime() > 2 * blinkie_time ) {
        blink_timer.Restart();
    }
}
void Console::RenderTypeSuggestions()
{
    if( input_line.size() == 0 || suggestion_map.size() == 0 ) {
        return;
    }

    const float x1 = x;
    const float x2 = x1 + suggestion_box_width + 2 * text_off_left;
    const float y1 = y + h;
    const float y2 = y1 + text_off_down + line_height * suggestion_map.size();

    sf::Shape box = sf::Shape::Rectangle( x1, y1, x2, y2,
        suggestion_back_color );

    Tree::Draw( box );

    int n = 0;
    for( StrMap::iterator it = suggestion_map.begin(); it != suggestion_map.end(); ++it )
    {
        if( n == sugg_pos ) {
            render_str.SetColor( fnt_highlight_color );
        }
        else {
            render_str.SetColor( fnt_suggestion_color );
        }

        render_str.SetText( it->first + ' ' + it->second );
        render_str.SetPosition( x + text_off_left, y + h + line_height * n );

        Tree::Draw( render_str );
        n++;
    }
}

void Console::RenderDebug()
{
    /*if( !showDebug->Val() ) return;

    fnt->SetColor( 0xffffffff );
    fnt->printf( x + w + text_off_left, y, HGETEXT_LEFT, "line_pos: %i history_pos: %i history_length: %i",
        input_line_pos, cmd_pos, cmd_history.size() );

    std::string s;
    if( sel_start == -1 || sel_length == 0 ) {
        s = "nop";
    }
    else {
        if( sel_length > 0 ) {
            s = input_line.substr( sel_start, sel_length );
        }
        else {
            s = input_line.substr( sel_start + sel_length, -sel_length );
        }
    }
    fnt->printf( x + w + text_off_left, y + line_height, HGETEXT_LEFT, "sel_start: %i sel_length: %i substr: %s",
        sel_start, sel_length, s.c_str() );
    */
}

