#pragma once

#include <vector>

#include "Tree/Sprite.hpp"
#include "Tile.hpp"

//just a regular tile
//so we don't have to create 1 million tiles just to have em look different
class SpriteTile : public Tile {
public:
    SpriteTile( Tree::Vec2i pos, Tree::Sprite _spr ) : Tile( pos ), spr( _spr )
    {
        spr.SetPos( pos.x, pos.y );
    }
    virtual ~SpriteTile() { }

    virtual void Draw( Tree::Vec2i p ) {
        spr.SetPos( p.x, p.y );
        spr.Draw();
    }
protected:
    Tree::Sprite spr;
};

//empty and black tile
class BlockTile : public Tile {
public:
    BlockTile( Tree::Vec2i pos ) : Tile ( pos )
    { }

    bool IsWalkable() { return false; }
    bool IsSeeThrough() { return false; }

    void Draw( Tree::Vec2i ) { }
};

class Floor : public Tile {
public:
    Floor( Tree::Vec2i pos );

    void Draw( Tree::Vec2i p );
private:
    sf::Sprite spr;
};

