#include "Tree/Butler.hpp"
#include "Floor.hpp"

Floor::Floor()
{
    spr = Tree::GetButler()->GetSprite( "floor" );
}

void Floor::Draw()
{
    spr->Draw();
}

