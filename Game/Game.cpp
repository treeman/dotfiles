#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/bind.hpp>

#include "Tree/Settings.hpp"
#include "Tree/Dator.hpp"
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

    boost::shared_ptr<Tree::SilentDator> next_lvl( new Tree::SilentDator( boost::bind(
        &World::NextLevel, world.get())));
    Tree::GetSettings()->RegisterPermVariable( "lvl_next", next_lvl );

    boost::shared_ptr<Tree::SilentDator> prev_lvl( new Tree::SilentDator( boost::bind(
        &World::PreviousLevel, world.get())));
    Tree::GetSettings()->RegisterPermVariable( "lvl_prev", prev_lvl );

    boost::shared_ptr<Tree::SilentDator> first_lvl( new Tree::SilentDator( boost::bind(
        &World::SetFirstLevel, world.get())));
    Tree::GetSettings()->RegisterPermVariable( "lvl_first", first_lvl );

    boost::shared_ptr<Tree::SilentDator> reset_lvl( new Tree::SilentDator( boost::bind(
        &World::ResetLevel, world.get())));
    Tree::GetSettings()->RegisterPermVariable( "lvl_reset", reset_lvl );
}

bool Game::HandleEvent( sf::Event &e )
{
    if( e.Type == sf::Event::KeyPressed ) {
        switch( e.Key.Code ) {
            case sf::Key::N:
                world->NextLevel();
                break;
            case sf::Key::P:
                world->PreviousLevel();
                break;
            case sf::Key::R:
                world->ResetLevel();
                break;
            case sf::Key::F:
                world->SetFirstLevel();
                break;
            default:
                break;
        }
    }
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

