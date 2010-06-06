#include "Tree/Game.hpp"
#include "Tree/Util.hpp"

#include "Help.hpp"

Help::Help()
{
    Tree::GetButler()->GetImage( "gfx/help.png" )->SetSmooth( false );
    spr.SetImage( *Tree::GetButler()->GetImage( "gfx/help.png" ) );
}
Help::~Help()
{
}

bool Help::HandleEvent( sf::Event &event )
{
    if( event.Type == sf::Event::KeyPressed
        || event.Type == sf::Event::MouseButtonPressed )
    {
        Tree::Game::Instance()->Pop();
    }
    return true;
}

void Help::Update( float dt )
{

}
void Help::Draw()
{
    Tree::Draw( spr );
}

