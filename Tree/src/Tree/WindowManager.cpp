#include <boost/bind.hpp>
#include <boost/function.hpp>

#include "Tree/WindowManager.hpp"
#include "Tree/Settings.hpp"

using Tree::WindowManager;

WindowManager::WindowManager() : window( new sf::RenderWindow() ), has_setup( false )
{
    width.reset( new Dator<int>( 800, boost::bind( &WindowManager::SetScreenWidth, this, _1 ) ) );
    height.reset( new Dator<int>( 600, boost::bind( &WindowManager::SetScreenHeight, this, _1 ) ) );
    bpp.reset( new Dator<int>( 32, boost::bind( &WindowManager::SetScreenBPP, this, _1 ) ) );
    is_windowed.reset( new Dator<bool>( true, boost::bind( &WindowManager::SetWindowed, this, _1 ) ) );
    title.reset( new Dator<std::string>( "W00t nuthing initialized!", boost::bind( &WindowManager::SetScreenTitle, this, _1 ) ) );

    Tree::GetSettings()->RegisterVariable( "video_screen_width", boost::weak_ptr<BaseDator>( width ) );
    Tree::GetSettings()->RegisterVariable( "video_screen_height", boost::weak_ptr<BaseDator>( height ) );
    Tree::GetSettings()->RegisterVariable( "video_screen_bpp", boost::weak_ptr<BaseDator>( bpp ) );
    Tree::GetSettings()->RegisterVariable( "video_screen_windowed", boost::weak_ptr<BaseDator>( is_windowed ) );
    Tree::GetSettings()->RegisterVariable( "video_caption_title", boost::weak_ptr<BaseDator>( title ) );

    has_setup = true;

    UpdateWindow();
}

WindowManager::~WindowManager()
{
    if( window->IsOpened() ) window->Close();
}

void WindowManager::UpdateWindow()
{
    //prevent from recreating the screen while setting initial values
    if( !has_setup ) return;

    int style;
    if( is_windowed->Val() ) {
        style = sf::Style::Close;
    }
    else {
        style = sf::Style::Fullscreen;
    }

    window->Create( sf::VideoMode( width->Val(), height->Val(), bpp->Val() ),
        title->Val(), style );
}

std::string WindowManager::SetWindowed( bool predicate )
{
    UpdateWindow();
    if( predicate ) {
        return "you're now surfing machine code in windowed mode";
    }
    else {
        return "you're now surfing machine code in fullscreen";
    }
}
std::string WindowManager::SetScreenWidth( int val )
{
    UpdateWindow();
    return "";
}
std::string WindowManager::SetScreenHeight( int val )
{
    UpdateWindow();
    return "";
}
std::string WindowManager::SetScreenBPP( int val )
{
    UpdateWindow();
    return "";
}
std::string WindowManager::SetScreenTitle( std::string val )
{
    UpdateWindow();
    return "setting window title to '" + val + "'";
}

