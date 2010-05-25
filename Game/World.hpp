#pragma once

#include "Tree/Graphics.hpp"

#include "Grid.hpp"
#include "Tile.hpp"
#include "Level.hpp"
#include "LevelLoader.hpp"

class World {
public:
    World();
    ~World();

    void SetFirstLevel();
    void NextLevel();
    void PreviousLevel();
    void ResetLevel();

    void Update( float dt );
    void Draw();
private:
    Grid grid;
    TileGrid tiles;

    sf::String lvl_str;

    void LoadLevel( Level &lvl );
    LevelLoader lvl_loader;
    Level *curr_lvl;
};

