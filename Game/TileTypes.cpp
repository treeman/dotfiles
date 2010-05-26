#include "Tree/Math.hpp"
#include "Tree/Butler.hpp"
#include "TileTypes.hpp"

Floor::Floor( Tree::Vec2i pos ) : Tile( pos )
{
    sprites.push_back( Tree::GetButler()->GetSprite( "floor0" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor1" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor2" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor3" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor4" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor5" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor6" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor7" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor8" ) );
    sprites.push_back( Tree::GetButler()->GetSprite( "floor9" ) );
}

void Floor::Draw( Tree::Vec2i p )
{
    int floor_type = math::clip( (int)(light * 10), 0, 10 );
    if( floor_type != 0 ) {
        sprites[floor_type - 1].SetPos( p );
        sprites[floor_type - 1].Draw();
    }
}

