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

Candle::Candle()
{
    mod.new_candle = true;
    mod.can_remove = true;
    spr = Tree::GetButler()->GetSprite( "candle" );
}

void Candle::Draw( Tree::Vec2i pos )
{
    spr.SetColor( sf::Color( 51, 51, 51, lights_func( lighted ) ) );
    spr.SetPos( pos );
    spr.Draw();
}

