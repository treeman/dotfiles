#include "Tree/Butler.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Settings.hpp"
#include "Tree/VisualDebug.hpp"

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
    return (float)Tree::GetTweaks()->GetNum( "girl_speed" );
}

void Girl::Update( float dt )
{
    UpdateMovement( dt );

    std::stringstream s;
    s << "pos: " << pos.x << " " << pos.y;
    Tree::Debug( s.str() );
    s.str("");

    s << "vel: " << vel.x << " " << vel.y;
    Tree::Debug( s.str() );
    s.str("");

    s << "stop: " << stop_pos.x << " " << stop_pos.y;
    Tree::Debug( s.str() );
    s.str("");

    s << "next: " << next_move.x << " " << next_move.y;
    Tree::Debug( s.str() );
    s.str("");

    s << "face_dir: " << face_dir.x << " " << face_dir.y;
    Tree::Debug( s.str() );
    s.str("");
}

void Girl::Draw()
{
    spr.SetPos( pos );
    spr.Draw();
}
