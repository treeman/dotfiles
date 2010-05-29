#pragma once

#include "TileObject.hpp"

class LightObject : public TileObject {
public:
    LightObject();
    void Draw( Tree::Vec2i ) { }
};

class Candle : public TileObject {
public:
    Candle();
    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite spr;
};

