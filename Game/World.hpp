#pragma once

#include "Tree/Graphics.hpp"

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
    TileGrid tiles;

    void LoadLevel( Level &lvl );
    LevelLoader lvl_loader;
    Level *curr_lvl;

    boost::shared_ptr<Girl> girl;

    bool IsValid( Tree::Vec2i p ) { return IsValid( p.x, p.y ); }
    bool IsValid( size_t x, size_t y );

    bool IsWalkable( Tree::Vec2i p ) { return IsWalkable( p.x, p.y ); }
    bool IsWalkable( int x, int y );

    bool IsCollision( Tree::Vec2i p, Tree::Rect bounds ) { return IsCollision( p.x, p.y ); }
    bool IsCollision( int x, int y, Tree::Rect bounds );

    bool IsSeeThrough( Tree::Vec2i p ) { return IsSeeThrough( p.x, p.y ); }
    bool IsSeeThrough( int x, int y );

    void UpdateCollisions( MovingObject &o );

    Tree::Vec2f ConvertToWorld( Tree::Vec2i grid_pos );
    Tree::Vec2i ConvertToGrid( Tree::Vec2f screen_pos );
    Tree::Vec2i ConvertToGridByCenter( Tree::Vec2f screen_pos );

    const int tile_size;

    void CenterCam( Tree::Vec2i world_pos );
    Tree::Vec2f ConvertToScreen( Tree::Vec2f world_pos );
    Tree::Vec2i cam_pos;

    void UpdateLight( Tree::Vec2i grid_pos, float power );
    void IncrLight( Tree::Vec2i grid_pos, float power ) {
        IncrLight( grid_pos.x, grid_pos.y, power );
    }
    void IncrLight( int x, int y, float power );
    void IncrLight( TilePtr tile, float power );

};

