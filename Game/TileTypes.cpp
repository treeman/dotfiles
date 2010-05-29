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
    spr = Tree::GetButler()->GetSprite( "floor" );
}

void Floor::Draw( Tree::Vec2i p )
{
    spr.SetColor( sf::Color( 51, 51, 51, light_func( light ) ) );
    spr.SetPos( p.x, p.y );
    spr.Draw();
}

