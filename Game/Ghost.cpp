#include "Tree/Butler.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Settings.hpp"
#include "Tree/Math.hpp"
#include "Tree/VisualDebug.hpp"

#include "Ghost.hpp"

Ghost::Ghost( bool has_light )
{
    lspr = Tree::GetButler()->GetSprite( "left_ghost" );
    rspr = Tree::GetButler()->GetSprite( "right_ghost" );
    uspr = Tree::GetButler()->GetSprite( "up_ghost" );
    dspr = Tree::GetButler()->GetSprite( "down_ghost" );

    Tree::GetSettings()->Register<bool>( "debug_ghost", false );

    Pause();
}

Ghost::~Ghost()
{

}

float Ghost::GetSpeed()
{
    return (float)Tree::GetTweaks()->GetNum( "ghost_speed" );
}
void Ghost::Pause()
{
    timer.Restart();
    const float max_pause = Tree::GetTweaks()->GetNum( "ghost_pause_limit" );
    pause_length = math::frandom( 0, max_pause );

    const float max_move = Tree::GetTweaks()->GetNum( "ghost_move_limit" );
    move_length = math::frandom( 0, max_move );
}
bool Ghost::IsPaused()
{
    return timer.GetTime() < pause_length;
}
bool Ghost::WantsPause()
{
    return timer.GetTime() > move_length;
}

void Ghost::Update( float dt )
{
    UpdateMovement( dt );
    light.Update( dt );

    if( Tree::GetSettings()->GetValue<bool>( "debug_ghost" ) ) {
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
}

void Ghost::Draw( Tree::Vec2i p )
{
    Tree::Sprite *spr = 0;
    if( FacesLeft() ) spr = &lspr;
    else if( FacesRight() ) spr = &rspr;
    else if( FacesUp() ) spr = &uspr;
    else if( FacesDown() ) spr = &dspr;

    if( spr ) {
        spr->SetPos( p );
        spr->Draw();
    }
}

