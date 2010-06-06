#include "Tree/Game.hpp"
#include "Tree/Util.hpp"

#include "Victory.hpp"

Victory::Victory()
{
    Tree::GetButler()->GetImage( "gfx/victory.png" )->SetSmooth( false );
    spr.SetImage( *Tree::GetButler()->GetImage( "gfx/victory.png" ) );

    victory = Tree::GetButler()->GetSound( "sfx/victory.wav" );
}
Victory::~Victory()
{
}

bool Victory::HandleEvent( sf::Event &event )
{
    if( event.Type == sf::Event::KeyPressed )
    {
        switch( event.Key.Code ) {
            case sf::Key::Y:
            case sf::Key::Return:
            case sf::Key::Space:
                Tree::Game::Instance()->Pop();
                break;
            default:
                Tree::Game::Instance()->Exit();
        }
    }
    return true;
}

void Victory::Update( float dt )
{
    if( victory.GetStatus() != sf::Sound::Playing ) {
        victory.Play();
    }
}
void Victory::Draw()
{
    Tree::Draw( spr );
}

