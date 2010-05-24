#include <boost/lexical_cast.hpp>

#include "Tree/GameDebug.hpp"
#include "Tree/Butler.hpp"
#include "Tree/Util.hpp"

using Tree::GameDebug;

GameDebug::GameDebug()
{
    show_mouse_pos.reset( new Dator<bool>( false ) );
    Tree::GetSettings()->RegisterVariable( "mouse_pos_show", boost::weak_ptr<BaseDator>( show_mouse_pos ) );

    show_fps.reset( new Dator<bool>( false ) );
    Tree::GetSettings()->RegisterVariable( "fps_show", boost::weak_ptr<BaseDator>( show_fps ) );

    fps.SetFont( *Tree::GetButler()->GetFont( "fnt/lucon.ttf" ) );
    fps.SetSize( 20 );
    fps.SetColor( sf::Color( 255, 255, 255 ) );
    fps.SetText( "hello" );
    fps.SetPosition( 5, 5 );
}

void GameDebug::Update( float )
{
    int my_fps = (int)Tree::GetFPS();
    std::string s = boost::lexical_cast<std::string>( my_fps );
    fps.SetText( s );
}
void GameDebug::Draw()
{
    if( show_fps->Val() ) {
        Tree::Draw( fps );
    }
}

