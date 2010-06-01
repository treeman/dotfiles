#pragma once

#include "TileObject.hpp"

class LightObject : public TileObject {
public:
    LightObject();
    void Draw( Tree::Vec2i ) { }
};

class Candle : public TileObject {
public:
    Candle( int size, bool is_lighted = false );
    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite spr;
};

class Teddy : public TileObject {
public:
    Teddy( bool is_lighted = false );
    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite spr;
};

