#pragma once

#include "Tree/Sprite.hpp"
#include "MovingObject.hpp"
#include "Drawable.hpp"
#include "Light.hpp"

class Girl : public MovingObject, public Drawable {
public:
    Girl();
    ~Girl();

    float GetSpeed();
    Light &GetLight() { return light; }

    void Update( float dt );
    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite spr;
    Light light;
};

