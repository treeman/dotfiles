#pragma once

#include "Tree/Graphics.hpp"

#include "Grid.hpp"
#include "Tile.hpp"
#include "Level.hpp"
#include "LevelLoader.hpp"
#include "Girl.hpp"

class World {
public:
    World();
    ~World();

    boost::shared_ptr<Girl> GetGirl();

    //void AddListener( WorldListener *l );

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

    boost::shared_ptr<Girl> girl;

    bool IsWalkable( GridPos p ) { return IsWalkable( p.x, p.y ); }
    bool IsWalkable( int x, int y );

    bool IsSeeThrough( GridPos p ) { return IsSeeThrough( p.x, p.y ); }
    bool IsSeeThrough( int x, int y );

    bool IsValid( GridPos p ) { return IsValid( p.x, p.y ); }
    bool IsValid( int x, int y );
};

