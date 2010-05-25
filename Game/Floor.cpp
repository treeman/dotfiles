#include "Tree/Butler.hpp"
#include "Tree/Log.hpp"
#include "Floor.hpp"

Floor::Floor( Tree::Vec2i pos ) : Tile( pos )
{
    spr = Tree::GetButler()->GetSprite( "floor" );
    spr.SetPos( pos.x, pos.y );
}

void Floor::Draw()
{
    spr.Draw();
}

