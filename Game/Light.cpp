#include "Tree/Math.hpp"
#include "Tree/VisualDebug.hpp"
#include "Tree/Log.hpp"
#include "Light.hpp"

#include <boost/lexical_cast.hpp>

Light::Light() : power(0), vel(0), length(0), curr(0),
    decline_vel(0), use_flicker( false )
{
    Reset();
}

void Light::SetLight( float f )
{
    power = f;
}
float Light::GetLight()
{
    return math::clip<float>( power + curr, 0, 1 );
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

    if( use_flicker ) {
        UpdateFlicker( dt );
    }
}

void Light::Reset()
{
    vel = 0;
    curr = 0;
    last_flicker.Start();
    pause = math::frandom( 0, 3 );
}
void Light::Start()
{
    length = math::frandom( - power / 2, power / 2 );
    vel = math::frandom( power / 2, 2 * power );
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

#if 1
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
#endif

}
