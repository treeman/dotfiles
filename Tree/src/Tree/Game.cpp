#include <time.h>

#include "Tree/Game.hpp"
#include "Tree/Settings.hpp"
#include "Tree/Log.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Butler.hpp"

using Tree::Game;

Game::Game() : exit_called( false ), fps( 0 ),
    fps_buff( 0 ), fps_frame_count( 0 )
{
    settings.reset( new Settings() );
    tweaks.reset( new Tweaks() );
    butler.reset( new Butler() );
    log_helper.reset( new LogHelper() );
}
Game::~Game()
{
    L_ << "The game is destroyed";
}

void Game::Draw( const sf::Drawable &obj )
{
    window->Draw( obj );
}
float Game::GetFPS()
{
    return fps;
}
const sf::Input &Game::GetInput()
{
    return window->GetInput();
}

void Game::Init( int width, int height, bool windowed, std::string title,
    std::string settings_file )
{

    srand( time( NULL ) );

    settings->SetValue( "video_screen_width", width );
    settings->SetValue( "video_screen_height", height );
    settings->SetValue( "video_screen_windowed", windowed );

    settings->SetValue( "sound_enabled", true );
    settings->SetValue( "stream_volume", 100 );
    settings->SetValue( "music_volume", 100 );
    settings->SetValue( "effect_volume", 100 );

    if( settings_file != "" ) {
        try {
            settings->ParseFile( settings_file );
        }
        catch( Error::file_not_found &e ) {
            L_ << "Oops, you've deleted the settings file '" + settings_file + "'";
            L_ << "God will not forgive you!!! :@";
        }
    }

    settings->SetValue( "video_screen_bpp", 32 );
    settings->SetValue( "video_caption_title", title );

    window_manager.reset( new Tree::WindowManager() );
    window = window_manager->GetWindow();

    window->SetFramerateLimit( 500 );

    //sound_manager.reset( new Tree::SoundManager() );

    input_chain.reset( new Tree::InputChain() );

    console.reset( new Tree::Console() );
    console->SetPriority( 0 );
    input_chain->AddHandler( console.get() );

    console->AddHistory( "starting up the ownage game environment" );
    console->AddHistory( "screen res: " +
        settings->GetSetting( "video_screen_width" ) + "x" +
        settings->GetSetting( "video_screen_height" ) + " bpp: " +
        settings->GetSetting( "video_screen_bpp" )
    );

    game_debug.reset( new Tree::GameDebug() );
}

void Game::Start()
{
    if( ShallExit() ) return;

    while (window->IsOpened())
    {
        //if we change state, change it now and not in the middle of things
        curr_state = Top();

        const float dt = window->GetFrameTime();
        UpdateFPS( dt );

        sf::Event event;
        while( window->GetEvent( event ) )
        {
            //pass down events in a chain
            if( input_chain->HandleEvent( event ) ) {
                curr_state->HandleEvent( event );
            }

            //if we close the window, quit gracefully
            if( event.Type == sf::Event::Closed ) {
                Exit();
                break;
            }
        }

        curr_state->Update( dt );
        console->Update( dt );
        game_debug->Update( dt );

        //exit as fast as we can, no need to render if we want to quit
        if( ShallExit() ) {
            return;
        }

        //begin render loop
        window->Clear();

        //we want to render console and debug on top of things
        curr_state->Draw();
        console->Render();
        game_debug->Draw();

        //actually draw everything
        window->Display();

        //necessary to call it here so we can log rendering stuff too
        //this will simply reset the one time frame log
        log_helper->EndofLoop();
    }
}

void Game::Pop()
{
    state_list.pop_front();
}
void Game::Push( boost::shared_ptr<GameState> state )
{
    state_list.push_front( state );
}
boost::shared_ptr<Tree::GameState> Game::Top()
{
    if( !state_list.empty() ) {
        return state_list.front();
    }
    else {
        boost::shared_ptr<GameState> empty;
        return empty;
    }
}

void Game::Exit()
{
    exit_called = true;
}
bool Game::ShallExit() const
{
    return state_list.empty() || exit_called;
}

void Game::UpdateFPS( float dt )
{
    //simply update each second
    //no need for anything more sophisticated
    fps_buff += dt;
    ++fps_frame_count;
    if( fps_buff > 1.0 ) {
        fps = fps_frame_count / fps_buff;
        fps_buff = fps_frame_count = 0;
    }
}

