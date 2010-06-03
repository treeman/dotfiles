#include "Tree/Butler.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Settings.hpp"
#include "Tree/VisualDebug.hpp"

#include "Girl.hpp"

Girl::Girl() : change_candle( false )
{
    spr = Tree::GetButler()->GetSprite( "girl" );
    Tree::GetSettings()->Register<bool>( "debug_girl", false );

    light.SetLightDecline( Tree::GetTweaks()->GetNum( "candle_decline" ) );
    light.SetLightSpread( 1 );
    light.SetFlicker( true );
}

Girl::~Girl()
{

}

float Girl::GetSpeed()
{
    return (float)Tree::GetTweaks()->GetNum( "girl_speed" );
}

void Girl::ChangeCandle()
{
    change_candle = true;
}
bool Girl::WantsCandleChange()
{
    return change_candle;
}

void Girl::Update( float dt )
{
    change_candle = false;

    UpdateMovement( dt );
    light.Update( dt );

    if( Tree::GetSettings()->GetValue<bool>( "debug_girl" ) ) {
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

void Girl::Draw( Tree::Vec2i p )
{
    spr.SetPos( p );
    spr.Draw();
}
