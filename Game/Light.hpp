#pragma once

#include "Tree/Timer.hpp"

//this'll flicker the light, like a candle (I hope)
class Light {
public:
    Light();

    void SetFlicker( bool flicker ) { use_flicker = flicker; }
    void SetLight( float power );
    float GetLight();

    void SetLightDecline( float vel );
    float GetLightDecline();

    void Update( float dt );
private:
    void Reset();
    void Start();

    void UpdateFlicker( float dt );
    float power;
    float vel;
    float length;
    float curr;

    Tree::Timer last_flicker;
    float pause;

    float decline_vel;
    bool use_flicker;
};

