#include "Tree/Math.hpp"
#include "Tree/VisualDebug.hpp"
#include "Tree/Log.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Settings.hpp"
#include "Light.hpp"

#include <boost/lexical_cast.hpp>

Light::Light() : power(0), vel(0), length(0), curr(0), spread(2),
    decline_vel(0), use_flicker( false )
{
    Reset();
}

void Light::SetLightPower( float f )
{
    power = f;
}
float Light::GetLightPower()
{
    return math::clip<float>( power + curr, 0, 1 );
}
float Light::GetRealLightPower()
{
    return power;
}
int Light::GetLightSpread()
{
    return spread;
}
void Light::SetLightSpread( int tiles )
{
    spread = tiles;
}
void Light::SetLightDecline( float vel )
{
    decline_vel = vel;
}
float Light::GetLightDecline()
{
    return decline_vel;
}
void Light::Update( float dt )
{
    power -= decline_vel * dt;
    if( power < 0 ) power = 0;

    if( use_flicker ) {
        UpdateFlicker( dt );
    }
}

void Light::Reset()
{
    vel = 0;
    curr = 0;
    last_flicker.Restart();
    const float max_pause = Tree::GetTweaks()->GetNum( "light_flicker_pause_limit" );
    pause = math::frandom( 0, max_pause );
}
void Light::Start()
{
    //const float l = ( power * 1 ) / 2;
    const float l = power * Tree::GetTweaks()->GetNum( "light_flicker_length" );
    length = math::frandom( - l, l );
    const float min_vel = Tree::GetTweaks()->GetNum( "light_flicker_min_vel" );
    const float max_vel = Tree::GetTweaks()->GetNum( "light_flicker_max_vel" );
    vel = math::frandom( power * min_vel, power * max_vel );
    curr = 0;
}

void Light::UpdateFlicker( float dt )
{
    if( vel == 0 ) {
        if( last_flicker.GetTime() > pause ) {
            Start();
        }
    }
    else {
        curr += vel * dt;

        if( ( length < 0 && curr >= 0 )
         || ( length > 0 && curr <= 0 ) )
        {
            Reset();
        }
        else if( ( length < 0 && vel < 0 && curr <= length )
            || ( length > 0 && vel > 0 && curr >= length ) )
        {
            vel = -vel;
            curr = length;
        }
    }

    if( Tree::GetSettings()->GetValue<bool>( "debug_light" ) ) {
        std::stringstream ss;
        ss << "power: " << power;
        Tree::Debug( ss.str() );
        ss.str("");

        ss << "curr: " << curr;
        Tree::Debug( ss.str() );
        ss.str("");

        ss << "vel: " << vel;
        Tree::Debug( ss.str() );
        ss.str("");

        ss << "time: " << last_flicker.GetTime();
        Tree::Debug( ss.str() );
        ss.str("");

        ss << "pause: " << pause;
        Tree::Debug( ss.str() );
        ss.str("");
    }
}

