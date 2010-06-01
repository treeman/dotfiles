#pragma once

#include "Tree/Vec2.hpp"
#include "Light.hpp"

struct ObjectMod {
    ObjectMod() : new_candle( false ), candle_power( 0 ),
        can_remove( false ), is_goal( false )
    { }

    bool new_candle;
    float candle_power;
    bool can_remove;
    bool is_goal;
};

class TileObject {
public:
    TileObject() : lighted( 0 )
    { }
    virtual ~TileObject() { }

    virtual bool IsWalkable() { return true; }
    virtual bool IsSeeThrough() { return true; }

    void SetPos( Tree::Vec2i p ) { pos = p; }
    Tree::Vec2i GetPos() const { return pos; }

    Light &GetLightSource() { return light; }

    float GetLight() { return lighted; }
    void SetLight( float l ) { lighted = l; }

    //world modifiers for walkover action
    ObjectMod GetMod() { return mod; }

    virtual void Update( float dt ) { light.Update( dt ); }
    virtual void Draw( Tree::Vec2i pos ) = 0;
protected:
    Tree::Vec2i pos;
    Light light;
    ObjectMod mod;
    float lighted;
};

