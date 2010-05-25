#pragma once

#include <boost/shared_ptr.hpp>
#include "Tree/Vec2.hpp"
#include "Tree/Sprite.hpp"

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

//just a regular tile
//so we don't have to create 1 million tiles just to have em look different
class SpriteTile : public Tile {
public:
    SpriteTile( Tree::Vec2i pos, Tree::Sprite _spr ) : Tile( pos ), spr( _spr )
    {
        spr.SetPos( pos.x, pos.y );
    }
    virtual ~SpriteTile() { }

    virtual void Draw() {
        spr.Draw();
    }
protected:
    Tree::Sprite spr;
};

