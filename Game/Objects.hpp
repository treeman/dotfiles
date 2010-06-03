#pragma once

#include "TileObject.hpp"

class LightObject : public TileObject {
public:
    LightObject( bool is_lighted = true );
    virtual ~LightObject() { }

    virtual void Draw( Tree::Vec2i ) { }
};

class SpriteObject : public LightObject {
public:
    SpriteObject( bool is_lighted = true );
    virtual ~SpriteObject() { }

    virtual void Draw( Tree::Vec2i );
protected:
    Tree::Sprite spr;
};

class Candle : public SpriteObject {
public:
    Candle( int size, bool is_lighted = false );
};

class Teddy : public SpriteObject {
public:
    Teddy( bool is_lighted = false );
};

class Door : public SpriteObject {
public:
    Door( bool is_lighted = false );

    bool IsDoor() { return true; }
    bool IsSeeThrough() { return true; }
};

class Key : public SpriteObject {
public:
    Key( bool is_lighted = false );
};

class Skeleton : public SpriteObject {
public:
    Skeleton( bool is_lighted = false );

    bool CanBlowOut() { return true; }
};

