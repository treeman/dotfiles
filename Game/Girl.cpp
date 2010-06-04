#include "Tree/Butler.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Settings.hpp"
#include "Tree/VisualDebug.hpp"

#include "Girl.hpp"

float l_func( float light )
{
    const int byte = light * 255;
    return byte;
}

Girl::Girl() : do_action( false ), lighted( 0 )
{
    lspr = Tree::GetButler()->GetSprite( "left_girl" );
    rspr = Tree::GetButler()->GetSprite( "right_girl" );
    uspr = Tree::GetButler()->GetSprite( "up_girl" );
    dspr = Tree::GetButler()->GetSprite( "down_girl" );

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

void Girl::DoAction()
{
    do_action = true;
}
bool Girl::WantsAction()
{
    return do_action;
}

void Girl::Update( float dt )
{
    do_action = false;

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
    Tree::Sprite *spr = 0;
    if( FacesLeft() ) spr = &lspr;
    else if( FacesRight() ) spr = &rspr;
    else if( FacesUp() ) spr = &uspr;
    else if( FacesDown() ) spr = &dspr;

    if( spr ) {
        spr->SetColor( sf::Color( 51, 51, 51, l_func( lighted ) ) );
        spr->SetPos( p );
        spr->Draw();
    }
}

