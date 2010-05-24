#pragma once

#include "Tree/Sprite.hpp"
#include "Tile.hpp"

class Floor : public Tile {
public:
    Floor();

    void Draw();
private:
    boost::shared_ptr<Sprite> spr;
};

