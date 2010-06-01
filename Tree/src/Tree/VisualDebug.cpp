#include "Tree/VisualDebug.hpp"
#include "Tree/Game.hpp"
#include "Tree/Util.hpp"

using Tree::VisualDebug;

void Tree::Debug( std::string message )
{
    GAME->VisualDebug( message );
}

VisualDebug::VisualDebug()
{
    str.SetFont( *Tree::GetButler()->GetFont( "fnt/arial.ttf", 10 ) );
    str.SetSize( 10 );
    str.SetColor( sf::Color( 234, 80, 178 ) );

    Tree::GetSettings()->Register<bool>( "debug_visual", false );
}
void VisualDebug::EndofLoop()
{
    strings.clear();
}

void VisualDebug::Push( std::string debug_message )
{
    strings.push_back( debug_message );
}
void VisualDebug::Draw()
{
    if( strings.empty() ) return;
    if( !Tree::GetSettings()->GetValue<bool>( "debug_visual" ) ) return;

    const float x = Tree::GetWindowWidth() - 100;
    const float y = 5;
    const float h = 10;

    for( size_t i = 0; i < strings.size(); ++i ) {
        str.SetText( strings[i] );
        str.SetPosition( x, y + i * h );
        Tree::Draw( str );
    }
}

