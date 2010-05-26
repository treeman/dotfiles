#pragma once

#include "Tree/Vec2.hpp"
#include "Light.hpp"

class Drawable {
public:
    Drawable()
    { }
    virtual ~Drawable() { }

    virtual void Draw( Tree::Vec2i pos ) = 0;
};

