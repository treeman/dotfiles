#include "Tree/Graphics.hpp"
#include "Tree/Game.hpp"

void Tree::Draw( const sf::Drawable &obj )
{
    Tree::Game::Instance()->Draw( obj );
}

