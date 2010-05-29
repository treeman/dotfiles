#pragma once

#include <boost/shared_ptr.hpp>
#include "Tree/Vec2.hpp"
#include "Tree/Sprite.hpp"
#include "Tree/Rect.hpp"

#include "Drawable.hpp"
#include "TileObject.hpp"

class Tile;

typedef boost::shared_ptr<Tile> TilePtr;
typedef std::vector<TilePtr> Tiles;
typedef std::vector<Tiles> TileGrid;

class Tile : public Drawable {
public:
    Tile( Tree::Vec2i _pos ) : pos( _pos )
    { }
    virtual ~Tile() { }

    float GetLight() const { return light; }
    void SetLight( float f ) { light = f; }

    Tree::Vec2i GetPos() const { return pos; }

    virtual bool IsWalkable() {
        if( attachment ) return attachment->IsWalkable();
        else return true;
    }
    virtual bool IsSeeThrough() {
        if( attachment ) return attachment->IsSeeThrough();
        else return true;
    }

    boost::shared_ptr<TileObject> GetAttachment() { return attachment; }
    virtual bool CanAttach() { return true; }
    void Attach( boost::shared_ptr<TileObject> o ) {
        if( CanAttach() ) attachment = o;
    }
    void Detach() { attachment.reset(); }

    virtual void Update( float dt ) { }
    virtual void Draw( Tree::Vec2i p ) = 0;
protected:
    Tree::Vec2i pos;
    float light;
    boost::shared_ptr<TileObject> attachment;
};

