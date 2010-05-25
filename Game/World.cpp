#include "Tree/Tweaks.hpp"
#include "Tree/Util.hpp"
#include "Tree/Butler.hpp"
#include "Tree/Log.hpp"

#include "World.hpp"

World::World() : curr_lvl( 0 )
{
    lvl_loader.LoadLevelFile( "levels.lua" );
    if( !lvl_loader.IsThereALevel() ) {
        throw( Error::logic_error( "There isn't a level present. Jumping ship." ));
    }

    lvl_str.SetFont( *Tree::GetButler()->GetFont( "fnt/consola.ttf", 10 ) );
    lvl_str.SetSize( 10 );
    lvl_str.SetPosition( 500, 10 );
    lvl_str.SetColor( sf::Color( 0, 0, 0 ) );

    girl.reset( new Girl() );

    SetFirstLevel();
}
World::~World()
{

}

boost::shared_ptr<Girl> World::GetGirl()
{
    return girl;
}

void World::SetFirstLevel()
{
    LoadLevel( lvl_loader.GetFirstLevel() );
}
void World::NextLevel()
{
    if( !curr_lvl->IsLast() ) {
        LoadLevel( curr_lvl->GetNext() );
    }
}
void World::PreviousLevel()
{
    if( !curr_lvl->IsFirst() ) {
        LoadLevel( curr_lvl->GetPrevious() );
    }
}
void World::ResetLevel()
{
    LoadLevel( *curr_lvl );
}

void World::Update( float dt )
{
    for( size_t x = 0; x < tiles.size(); ++x ) {
        for( size_t y = 0; y < tiles[x].size(); ++y ) {
            tiles[x][y]->Update( dt );
        }
    }
    girl->Update( dt );
}

void World::Draw()
{
    for( size_t x = 0; x < tiles.size(); ++x ) {
        for( size_t y = 0; y < tiles[x].size(); ++y ) {
            tiles[x][y]->Draw();
        }
    }
    Tree::Draw( lvl_str );

    girl->Draw();
}

void World::LoadLevel( Level &lvl )
{
    tiles = lvl_loader.CreateTiles( lvl );
    lvl_str.SetText( lvl.GetName() );

    curr_lvl = &lvl;
    girl->SetPos( grid.ConvertToScreen( GridPos( 5, 5 ) ) );
}

bool World::IsWalkable( int x, int y )
{
    if( !IsValid( x, y ) ) return false;
    else return tiles[x][y]->IsWalkable();
}
bool World::IsSeeThrough( int x, int y )
{
    if( !IsValid( x, y ) ) return false;
    else {
        return tiles[x][y]->IsSeeThrough();
    }
}
bool World::IsValid( int x, int y )
{
    return x >= 0 && x < grid.GetColumns() && y >= 0 && y < grid.GetRows();
}

