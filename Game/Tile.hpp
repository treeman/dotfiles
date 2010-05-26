#pragma once

#include <boost/shared_ptr.hpp>
#include "Tree/Vec2.hpp"
#include "Tree/Sprite.hpp"
#include "Tree/Rect.hpp"

#include "Drawable.hpp"

class Tile;

typedef boost::shared_ptr<Tile> TilePtr;
typedef std::vector<TilePtr> Tiles;
typedef std::vector<Tiles> TileGrid;

class Tile : public Drawable {
public:
    Tile( Tree::Vec2i _pos ) : pos( _pos )
    { }
    virtual ~Tile() { }

    Tree::Vec2i GetPos() const { return pos; }

    virtual bool IsWalkable() { return true; }
    virtual bool IsSeeThrough() { return true; }

    virtual void Update( float dt ) { }
    virtual void Draw( Tree::Vec2i p ) = 0;
protected:
    Tree::Vec2i pos;
};
