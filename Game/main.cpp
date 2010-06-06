#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>

#include "Tree/Game.hpp"
#include "Tree/Log.hpp"
#include "Tree/Logo.hpp"

#include "Game.hpp"
#include "Help.hpp"
#include "Victory.hpp"

int main( int argc, char *argv[] )
{
    //init the boost logs
    Tree::init_logs();

    try {
        //create the game
        Tree::Game::Instance()->Init(
              800 //width
            , 600 //height
            , true //windowed
            , "Where's Teddy?" //window title
            , "settings.ini" //settings file
        );

        //push your gamestates here
        {
            boost::shared_ptr<Tree::GameState> state;
            state.reset( new Game() );
            Tree::Game::Instance()->Push( state );
            state.reset( new Help() );
            Tree::Game::Instance()->Push( state );
            state.reset( new Tree::Logo() );
            Tree::Game::Instance()->Push( state );
        }

        //start the main game loop
        Tree::Game::Instance()->Start();
    }
    catch( std::exception &e )
    {
        L_ << "Fatal exception: " << e.what();
    }
    catch( ... )
    {
        L_ << "Fatal unknown exception caught!";
    }

    //destroy the game
    Tree::Game::Instance()->Destroy();

    return 0;
}

