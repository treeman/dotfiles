#include "World.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Utils.hpp"

World::World()
{
    int tile_size = (int)Tree::GetTweaks()->GetDouble( "tile_size" );
    if( tile_size ) {
        int columns = Tree::GetWindowWidth() / tile_size;
        int rows = Tree::GetWindowHeight() / tile_size;
        grid.Set( 0, tile_size, columns, 0, tile_size, rows );
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

