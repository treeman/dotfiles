#pragma once

#include "Tree/Sprite.hpp"
#include "Tile.hpp"

class Floor : public Tile {
public:
    Floor( Tree::Vec2i pos );

    void Draw();
private:
    Tree::Sprite spr;
};

