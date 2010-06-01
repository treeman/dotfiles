#pragma once

#include "Tree/Vec2.hpp"
#include "Tile.hpp"
#include "Ghost.hpp"

struct LevelResources {
    Tree::Vec2i girl_pos;
    TileGrid tiles;

    typedef std::vector<boost::shared_ptr<Ghost> > Ghosts;
    Ghosts ghosts;
};

