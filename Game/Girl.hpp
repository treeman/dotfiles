#pragma once

#include "Tree/Sprite.hpp"
#include "MovingObject.hpp"

class Girl : public MovingObject {
public:
    Girl();
    ~Girl();

    float GetSpeed();

    void Update( float dt );
    void Draw();
private:
    Tree::Sprite spr;
};
