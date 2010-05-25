#include "Tree/Tweaks.hpp"
#include "Tree/Util.hpp"
#include "Tree/Log.hpp"

#include "World.hpp"
#include "Floor.hpp"

World::World()
{
    size_t tile_size = (int)Tree::GetTweaks()->GetDouble( "tile_size" );
    if( tile_size ) {
        size_t columns = Tree::GetWindowWidth() / tile_size;
        size_t rows = Tree::GetWindowHeight() / tile_size;
        grid.Set( 0, tile_size, columns, 0, tile_size, rows );
    }

    for( size_t x = 0; x < grid.GetColumns(); ++x ) {
        Tiles column;
        for( size_t y = 0; y < grid.GetRows(); ++y ) {
            TilePtr tile;
            Tree::Vec2i pos = grid.ConvertToScreen( GridPos( x, y ) );
            tile.reset( new Floor( pos ) );
            column.push_back( tile );
        }
        tiles.push_back( column );
    }
}
World::~World()
{

}

void World::Reset()
{

}
void World::Start()
{

}
//void World::LoadLevel( Level &lvl )
//{
//
//}

void World::Update( float dt )
{
    for( size_t x = 0; x < tiles.size(); ++x ) {
        for( size_t y = 0; y < tiles[x].size(); ++y ) {
            tiles[x][y]->Update( dt );
        }
    }
}

void World::Draw()
{
    for( size_t x = 0; x < tiles.size(); ++x ) {
        for( size_t y = 0; y < tiles[x].size(); ++y ) {
            tiles[x][y]->Draw();
        }
    }
}

