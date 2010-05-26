#include "Tree/Math.hpp"
#include "Tree/Butler.hpp"
#include "TileTypes.hpp"

float light_func( float light )
{
    const int byte = light * 255;
    return byte;
}

Floor::Floor( Tree::Vec2i pos ) : Tile( pos )
{
    spr.SetImage( *Tree::GetButler()->GetImage( "gfx/floor.png" ) );
}

void Floor::Draw( Tree::Vec2i p )
{
    spr.SetColor( sf::Color( 51, 51, 51, light_func( light ) ) );
    spr.SetPosition( p.x, p.y );
    Tree::Draw( spr );
}

