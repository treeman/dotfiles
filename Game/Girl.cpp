#include "Tree/Butler.hpp"
#include "Tree/Tweaks.hpp"

#include "Girl.hpp"

Girl::Girl()
{
    spr = Tree::GetButler()->GetSprite( "girl" );
}

Girl::~Girl()
{

}

float Girl::GetSpeed()
{
    return (float)Tree::GetTweaks()->GetDouble( "girl_speed" );
}

void Girl::Update( float dt )
{
    UpdateMovement( dt );
}

void Girl::Draw()
{
    spr.SetPos( pos );
    spr.Draw();
}
