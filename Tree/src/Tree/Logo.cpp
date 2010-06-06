#include "Tree/Logo.hpp"
#include "Tree/Game.hpp"
#include "Tree/Util.hpp"

using Tree::Logo;

Logo::Logo()
{
    Tree::GetButler()->GetImage( "gfx/treelogo.png" )->SetSmooth( false );
    logo.SetImage( *Tree::GetButler()->GetImage( "gfx/treelogo.png" ) );

    const int w = Tree::GetWindowWidth();
    const int h = Tree::GetWindowHeight();

    logo.SetPosition( ( w - 300 ) / 2, ( h - 200 ) / 2 - 50 );

    fade = 0;
    fade_dir = -1;

    t.Start();
}
Logo::~Logo()
{
}

bool Logo::HandleEvent( sf::Event &event )
{
    if( event.Type == sf::Event::KeyPressed
        || event.Type == sf::Event::MouseButtonPressed )
    {
        Tree::Game::Instance()->Pop();
    }
    return true;
}

void Logo::Update( float dt )
{
    const float fade_time = 1.0;
    const float fade_pause = 2.0;

    if( fade_dir == 1 )
    {
        fade = (int)( 255 * t.GetTime() / fade_time );
        if( fade > 255 ) {
            fade = 255;
            Tree::Game::Instance()->Pop();
            return;
        }
    }
    else if( fade_dir == -1 )
    {
        fade = 255 - (int)( 255 * t.GetTime() / fade_time );
        if( fade < 0 ) {
            fade = 0;
            fade_dir = 0;
            t.Restart();
        }
    }
    else if( fade_dir == 0 ) {
        if( t.GetTime() > fade_pause ) {
            fade_dir = 1;
            t.Restart();
        }
    }
}
void Logo::Draw()
{
    logo.SetColor( sf::Color( 255, 255, 255, 255 - fade ) );
    Tree::Draw( logo );
}

