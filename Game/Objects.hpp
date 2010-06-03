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

class Door : public TileObject {
public:
    Door( bool is_lighted = false );

    bool IsDoor() { return true; }
    bool IsSeeThrough() { return true; }

    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite spr;
    int key_num;
};

class Key : public TileObject {
public:
    Key( bool is_lighted = false );

    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite spr;
};

class Skeleton : public TileObject {
public:
    Skeleton( bool is_lighted = false );

    bool CanBlowOut() { return true; }

    void Draw( Tree::Vec2i pos );
private:
    Tree::Sprite spr;
};

