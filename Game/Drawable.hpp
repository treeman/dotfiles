#pragma once

#include "Tree/Vec2.hpp"

class Drawable {
public:
    Drawable() : light( 0 )
    { }
    virtual ~Drawable() { }

    float GetLight() const { return light; }
    void SetLight( float l ) { light = l; }

    virtual void Draw( Tree::Vec2i pos ) = 0;
protected:
    float light;
};

