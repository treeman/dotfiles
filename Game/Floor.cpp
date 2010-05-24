#include "Tree/Butler.hpp"
#include "Tree/Log.hpp"
#include "Floor.hpp"

Floor::Floor( Tree::Vec2i pos ) : Tile( pos )
{
    spr = Tree::GetButler()->GetSprite( "floor" );
}

void Floor::Draw()
{
    //L_ << "drawing: " << pos;
    //spr->SetPosition( 40, 40 );
    spr->SetColor( sf::Color( 100, 100, 100, 100 ) );
    spr->SetPosition( 500, 500 );
    spr->Draw();
}
