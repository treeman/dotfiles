#pragma once

#include <boost/shared_ptr.hpp>
#include "Tree/Vec2.hpp"
#include "Tree/Sprite.hpp"
#include "Tree/Rect.hpp"

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

    virtual bool IsWalkable() { return true; }
    virtual bool IsSeeThrough() { return true; }

    virtual Tree::Rect Bounds() const {
        return Tree::Rect( pos.x, pos.y, 30, 30 );
    }

    virtual void Update( float dt ) { }
    virtual void Draw() = 0;
protected:
    Tree::Vec2i pos;
};
