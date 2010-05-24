#pragma once

#include "Grid.hpp"
#include "Tile.hpp"

class World {
public:
    World();
    ~World();

    void Reset();
    void Start();
    //void LoadLevel( Level &lvl );

    void Update( float dt );
    void Draw();
private:
    Grid grid;

    TileGrid tiles;
};

