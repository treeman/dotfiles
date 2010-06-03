#include "Tree/Tweaks.hpp"
#include "Tree/Butler.hpp"
#include "Objects.hpp"

float lights_func( float light )
{
    const int byte = light * 255;
    return byte;
}

LightObject::LightObject( bool is_lighted )
{
    if( is_lighted ) {
        light.SetLightPower( Tree::GetTweaks()->GetNum( "small_light_power" ) );
        light.SetLightSpread( Tree::GetTweaks()->GetNum( "small_light_spread" ) );
        light.SetFlicker( true );
    }
}

SpriteObject::SpriteObject( bool is_lighted ) :
    LightObject( is_lighted )
{ }

void SpriteObject::Draw( Tree::Vec2i pos )
{
    spr.SetColor( sf::Color( 51, 51, 51, lights_func( lighted ) ) );
    spr.SetPos( pos );
    spr.Draw();
}

Candle::Candle( int size, bool is_lighted )
{
    mod.new_candle = true;
    if( size ) {
        spr = Tree::GetButler()->GetSprite( "candle" );
        mod.candle_power = Tree::GetTweaks()->GetNum( "candle_power" );
    }
    else {
        spr = Tree::GetButler()->GetSprite( "small_candle" );
        mod.candle_power = Tree::GetTweaks()->GetNum( "small_candle_power" );
    }
    mod.can_remove = true;

    if( is_lighted ) {
        light.SetLightPower( Tree::GetTweaks()->GetNum( "small_light_power" ) );
        light.SetLightSpread( Tree::GetTweaks()->GetNum( "small_light_spread" ) );
        light.SetFlicker( true );
    }
}

Teddy::Teddy( bool is_lighted )
{
    spr = Tree::GetButler()->GetSprite( "teddy" );
    mod.can_remove = true;
    mod.is_goal = true;

    if( is_lighted ) {
        light.SetLightPower( Tree::GetTweaks()->GetNum( "small_light_power" ) );
        light.SetLightSpread( Tree::GetTweaks()->GetNum( "small_light_spread" ) );
        light.SetFlicker( true );
    }
}

Door::Door( bool is_lighted )
{
    spr = Tree::GetButler()->GetSprite( "door" );
    mod.can_remove = true;

    if( is_lighted ) {
        light.SetLightPower( Tree::GetTweaks()->GetNum( "small_light_power" ) );
        light.SetLightSpread( Tree::GetTweaks()->GetNum( "small_light_spread" ) );
        light.SetFlicker( true );
    }
}

Key::Key( bool is_lighted )
{
    spr = Tree::GetButler()->GetSprite( "key" );
    mod.can_remove = true;
    mod.is_key = true;

    if( is_lighted ) {
        light.SetLightPower( Tree::GetTweaks()->GetNum( "small_light_power" ) );
        light.SetLightSpread( Tree::GetTweaks()->GetNum( "small_light_spread" ) );
        light.SetFlicker( true );
    }
}

Skeleton::Skeleton( bool is_lighted )
{
    spr = Tree::GetButler()->GetSprite( "skeleton" );

    if( is_lighted ) {
        light.SetLightPower( Tree::GetTweaks()->GetNum( "small_light_power" ) );
        light.SetLightSpread( Tree::GetTweaks()->GetNum( "small_light_spread" ) );
        light.SetFlicker( true );
    }
}

