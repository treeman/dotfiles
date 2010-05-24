#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>

#include "Tree/Log.hpp"
#include "Tree/Butler.hpp"
#include "Tree/Tweaks.hpp"
#include "Game.hpp"

Game::Game()
{
    Tree::GetTweaks()->Load( "magic_numbers.lua" );

    t.Start();
    st.Start();
    st.SetSpeed( 2 );

    time_str.SetFont( *Tree::GetButler()->GetFont( "fnt/consola.ttf" ) );
    time_str.SetSize( 14 );
    time_str.SetColor( sf::Color( 255, 255, 255 ) );

    bag.reset( new Tree::ShuffleBag<int>() );

    for( int i = 0; i < Tree::GetTweaks()->GetDouble( "boxes" ); i++ ) {
        bag->Add( i );
    }
    ShuffleNext();

    shuffle_str.SetFont( *Tree::GetButler()->GetFont( "fnt/consola.ttf", 10 ) );
    shuffle_str.SetSize( 10 );
    shuffle_str.SetColor( sf::Color( 255, 255, 255 ) );

    Tree::GetButler()->LoadSprites( "sprites.lua" );

    dude = Tree::GetButler()->GetSprite( "dude" );
    dude->SetPosition( 230, 200 );

    girl = Tree::GetButler()->GetSprite( "girl" );
    girl->SetPosition( 400, 150 );

    background.SetImage( *Tree::GetButler()->GetImage( "gfx/dude.png" ) );
    background.SetPosition( 0, 0 );
}

bool Game::HandleEvent( sf::Event &e )
{
    if( e.Type == sf::Event::KeyPressed ) {
        switch( e.Key.Code ) {
            case( sf::Key::Return ):
                t.Restart();
                st.Restart();
                break;
            case sf::Key::Space:
                if( t.IsPaused() ) {
                    t.Start();
                }
                else {
                    t.Pause();
                }
                if( st.IsPaused() ) {
                    st.Start();
                }
                else {
                    st.Pause();
                }
                break;
            case sf::Key::G:
                t.SetTime( 10.0 );
                st.SetTime( 10.0 );
                break;
            case sf::Key::Num1:
                st.SetSpeed( 0.5 );
                break;
            case sf::Key::Num2:
                st.SetSpeed( 1.0 );
                break;
            case sf::Key::Num3:
                st.SetSpeed( 2.0 );
                break;
            case sf::Key::S:
                ShuffleNext();
                break;
            default:
                break;
        }
    }
    return true;
}

void Game::Update( float dt )
{

}
void Game::Draw()
{
    Tree::Draw( background );

    time_str.SetText( boost::lexical_cast<std::string>( t.GetTime() ) );
    time_str.SetPosition( 50, 5 );
    Tree::Draw( time_str );

    time_str.SetText( boost::lexical_cast<std::string>( st.GetTime() ) );
    time_str.SetPosition( 50, 15 );
    Tree::Draw( time_str );

    int n = 1;
    const float h = 10;
    for( Ints::iterator it = bagged.begin(); it != bagged.end(); ++it, ++n )
    {
        shuffle_str.SetPosition( 10, 30 + h * n );
        shuffle_str.SetText( boost::lexical_cast<std::string>( *it ) );
        Tree::Draw( shuffle_str );
    }

    n = 1;
    for( Ints::iterator it = rest.begin(); it != rest.end(); ++it, ++n )
    {
        shuffle_str.SetPosition( 30, 30 + h * n );
        shuffle_str.SetText( boost::lexical_cast<std::string>( *it ) );
        Tree::Draw( shuffle_str );
    }

    Tree::Draw( *dude );
    Tree::Draw( *girl );
}

void Game::ShuffleNext()
{
    latest = bag->Get();
    bagged = bag->GetBag();
    rest = bag->GetRest();
}

