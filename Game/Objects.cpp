#include "Tree/Tweaks.hpp"
#include "Tree/Butler.hpp"
#include "Objects.hpp"

float lights_func( float light )
{
    const int byte = light * 255;
    return byte;
}

LightObject::LightObject()
{
    light.SetLightPower( Tree::GetTweaks()->GetNum( "small_light_power" ) );
    light.SetLightSpread( Tree::GetTweaks()->GetNum( "small_light_spread" ) );
    light.SetFlicker( true );
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

void Candle::Draw( Tree::Vec2i pos )
{
    spr.SetColor( sf::Color( 51, 51, 51, lights_func( lighted ) ) );
    spr.SetPos( pos );
    spr.Draw();
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
void Teddy::Draw( Tree::Vec2i pos )
{
    spr.SetColor( sf::Color( 51, 51, 51, lights_func( lighted ) ) );
    spr.SetPos( pos );
    spr.Draw();
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
void Door::Draw( Tree::Vec2i pos )
{
    spr.SetColor( sf::Color( 51, 51, 51, lights_func( lighted ) ) );
    spr.SetPos( pos );
    spr.Draw();
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
void Key::Draw( Tree::Vec2i pos )
{
    spr.SetColor( sf::Color( 51, 51, 51, lights_func( lighted ) ) );
    spr.SetPos( pos );
    spr.Draw();
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
void Skeleton::Draw( Tree::Vec2i pos )
{
    spr.SetColor( sf::Color( 51, 51, 51, lights_func( lighted ) ) );
    spr.SetPos( pos );
    spr.Draw();
}

