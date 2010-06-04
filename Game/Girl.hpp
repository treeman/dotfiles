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
    Light &GetLightSource() { return light; }

    void DoAction();
    bool WantsAction();

    float GetLight() { return lighted; }
    void SetLight( float l ) { lighted = l; }

    void Update( float dt );
    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite lspr;
    Tree::Sprite rspr;
    Tree::Sprite uspr;
    Tree::Sprite dspr;

    Light light;
    bool do_action;

    float lighted;
};

