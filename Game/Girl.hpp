#pragma once

#include "Tree/Sprite.hpp"
#include "MovingObject.hpp"
#include "Drawable.hpp"

class Girl : public MovingObject, public Drawable {
public:
    Girl();
    ~Girl();

    float GetSpeed();

    void Update( float dt );
    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite spr;
};
