#pragma once

#include <vector>

#include "Tree/Sprite.hpp"
#include "Tree/Vec2.hpp"

#include "MovingObject.hpp"
#include "Drawable.hpp"
#include "Light.hpp"

class Ghost : public MovingObject, public Drawable {
public:
    Ghost( bool has_light = false );
    ~Ghost();

    float GetSpeed();
    Light &GetLightSource() { return light; }

    typedef std::vector<Tree::Vec2i> Dirs;
    void SetValidDirections( Dirs dirs ) { valid_dirs = dirs; }
    Dirs GetValidDirections() { return valid_dirs; }

    void Pause();
    bool IsPaused();
    bool WantsPause();

    void Update( float dt );
    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite lspr;
    Tree::Sprite rspr;
    Tree::Sprite uspr;
    Tree::Sprite dspr;

    Light light;

    Dirs valid_dirs;

    Tree::Timer timer;
    float pause_length;
    float move_length;
};

