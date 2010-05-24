#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>

#include "Tree/Log.hpp"
#include "Tree/Butler.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Vec2.hpp"
#include "Game.hpp"

Game::Game()
{
    Tree::GetButler()->LoadSprites( "sprites.lua" );
    Tree::GetTweaks()->Load( "magic_numbers.lua" );

    background.SetImage( *Tree::GetButler()->GetImage( "gfx/dude.png" ) );
    background.SetPosition( 0, 0 );

    world.reset( new World() );
}

bool Game::HandleEvent( sf::Event &e )
{
    return true;
}

void Game::Update( float dt )
{
    world->Update( dt );
}
void Game::Draw()
{
    Tree::Draw( background );

    world->Draw();
}
