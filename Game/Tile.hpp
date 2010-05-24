#pragma once

#include <boost/shared_ptr.hpp>
#include "Tree/Vec2.hpp"

class Tile;

typedef boost::shared_ptr<Tile> TilePtr;
typedef std::vector<TilePtr> Tiles;
typedef std::vector<Tiles> TileGrid;

class Tile {
public:
    Tile( Tree::Vec2i _pos ) : pos( _pos )
    { }
    virtual ~Tile() { }

    Tree::Vec2i GetPos() const { return pos; }

    virtual void Update( float dt ) { }
    virtual void Draw() = 0;
protected:
    Tree::Vec2i pos;
};

