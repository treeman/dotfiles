#include "Tree/Tweaks.hpp"
#include "Tree/Util.hpp"
#include "Tree/Butler.hpp"
#include "Tree/Log.hpp"
#include "Tree/Math.hpp"
#include "Tree/VisualDebug.hpp"
#include "Tree/Settings.hpp"

#include "World.hpp"

World::World() : curr_lvl( 0 ),
    tile_size( (int)Tree::GetTweaks()->GetNum( "tile_size" ) )
{
    lvl_loader.LoadLevelFile( "levels.lua" );
    if( !lvl_loader.IsThereALevel() ) {
        throw( Error::logic_error( "There isn't a level present. Jumping ship." ));
    }

    girl.reset( new Girl() );

    SetFirstLevel();

    Tree::GetSettings()->Register<bool>( "fow", true );

    girl->GetLight().SetLight( 0.6 );
    girl->GetLight().SetLightDecline( 0.01 );
    girl->GetLight().SetFlicker( true );
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
    const bool use_fow = Tree::GetSettings()->GetValue<bool>( "fow" );
    for( size_t x = 0; x < tiles.size(); ++x ) {
        for( size_t y = 0; y < tiles[x].size(); ++y ) {
            tiles[x][y]->Update( dt );
            if( use_fow ) {
                tiles[x][y]->SetLight( 0 );
            }
            else {
                tiles[x][y]->SetLight( 0.9 );
            }
        }
    }
    girl->Update( dt );
    UpdateCollisions( *girl );

    const Tree::Vec2f pos = girl->GetPos();
    const Tree::Vec2f center( pos.x + tile_size / 2, pos.y + tile_size / 2 );

    UpdateLight( ConvertToGridByCenter( girl->GetPos() ),
        girl->GetLight().GetLight() );

    CenterCam( center );

    const sf::Input *input = &Tree::GetInput();

    const Tree::Vec2f mpos( input->GetMouseX(), input->GetMouseY() );
    const Tree::Vec2i grid_pos = ConvertToGrid( mpos );

    std::stringstream ss;
    ss << "Tile: " << grid_pos;

    if( !IsValid( grid_pos ) ) {
        ss << "Not valid";
    }
    else if( !IsWalkable( grid_pos ) ) {
        ss << "Not walkable";
    }
    else if( !IsSeeThrough( grid_pos ) ) {
        ss << "Not see through either!";
    }
    else {
        ss << "It's see through and walkable =)";
    }
    Tree::Debug( ss.str() );
    Tree::Debug( curr_lvl->GetName() );

    ss.str("");
    ss << "cam_pos: " << cam_pos;
    Tree::Debug( ss.str() );
}

void World::Draw()
{
    for( size_t x = 0; x < tiles.size(); ++x ) {
        for( size_t y = 0; y < tiles[x].size(); ++y ) {
            tiles[x][y]->Draw( ConvertToScreen( tiles[x][y]->GetPos() ) );
        }
    }

    girl->Draw( ConvertToScreen( girl->GetPos() ) );
}

void World::LoadLevel( Level &lvl )
{
    LevelResources resources = lvl_loader.CreateResources( lvl );

    tiles = resources.tiles;
    girl->SetPos( resources.girl_pos );

    curr_lvl = &lvl;
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
bool World::IsValid( size_t x, size_t y )
{
    return x >= 0 && x < tiles.size() && y >= 0 && y < tiles[x].size();
}

void World::UpdateCollisions( MovingObject &o )
{
    const Tree::Vec2f pos = o.GetPos();
    const Tree::Vec2f center( pos.x + tile_size / 2, pos.y + tile_size / 2 );
    const Tree::Vec2i grid_pos = ConvertToGrid( center );
    const Tree::Vec2f screen_pos = ConvertToWorld( grid_pos );
    const Tree::Rect bounds( pos, tile_size, tile_size );

    //check the left
    if( !IsWalkable( grid_pos.x - 1, grid_pos.y ) ) {
        const float x_limit = ConvertToWorld( grid_pos ).x;
        if( pos.x < x_limit ) {
            Tree::Vec2f new_pos( x_limit, pos.y );
            o.SetPos( new_pos );
            o.SetVel( Tree::Vec2f::zero );
        }
    }
    //check the right
    if( !IsWalkable( grid_pos.x + 1, grid_pos.y ) ) {
        const float x_limit = ConvertToWorld( grid_pos ).x;
        if( pos.x > x_limit ) {
            Tree::Vec2f new_pos( x_limit, pos.y );
            o.SetPos( new_pos );
            o.SetVel( Tree::Vec2f::zero );
        }
    }
    //check above
    if( !IsWalkable( grid_pos.x, grid_pos.y - 1 ) ) {
        const float y_limit = ConvertToWorld( grid_pos ).y;
        if( pos.y < y_limit ) {
            Tree::Vec2f new_pos( pos.x, y_limit );
            o.SetPos( new_pos );
            o.SetVel( Tree::Vec2f::zero );
        }
    }
    //check down
    if( !IsWalkable( grid_pos.x, grid_pos.y + 1 ) ) {
        const float y_limit = ConvertToWorld( grid_pos ).y;
        if( pos.y > y_limit ) {
            Tree::Vec2f new_pos( pos.x, y_limit );
            o.SetPos( new_pos );
            o.SetVel( Tree::Vec2f::zero );
        }
    }

    std::stringstream s;

    s << "orig: " << pos.x << " " << pos.y;
    Tree::Debug( s.str() );
    s.str("");

    s << "center " << center.x << " " << center.y;
    Tree::Debug( s.str() );
    s.str("");

    s << "grid_pos " << grid_pos.x << " " << grid_pos.y;
    Tree::Debug( s.str() );
    s.str("");

    s << "screen " << screen_pos.x << " " << screen_pos.y;
    Tree::Debug( s.str() );
    s.str("");
}

Tree::Vec2f World::ConvertToWorld( Tree::Vec2i grid_pos )
{
    return Tree::Vec2f( grid_pos.x * tile_size, grid_pos.y * tile_size );
}
Tree::Vec2i World::ConvertToGrid( Tree::Vec2f screen_pos )
{
    return Tree::Vec2i(
        (int)( screen_pos.x / tile_size ),
        (int)( screen_pos.y / tile_size ) );
}
Tree::Vec2i World::ConvertToGridByCenter( Tree::Vec2f screen_pos )
{
    const Tree::Vec2f center( screen_pos.x + tile_size / 2,
        screen_pos.y + tile_size / 2 );
    return ConvertToGrid( center );
}

void World::CenterCam( Tree::Vec2i world_pos )
{
    cam_pos = world_pos;
}
Tree::Vec2f World::ConvertToScreen( Tree::Vec2f world_pos )
{
    const Tree::Vec2f p = world_pos - cam_pos;
    const int cx = Tree::GetWindowWidth() / 2;
    const int cy = Tree::GetWindowHeight() / 2;
    return Tree::Vec2f( p.x + cx, p.y + cy );
}

void World::UpdateLight( Tree::Vec2i grid_pos, float power )
{
    IncrLight( grid_pos, power );
    Tree::Vec2i check_pos;

    UpdateLight( grid_pos.x - 1, grid_pos.y, grid_pos, power );
    UpdateLight( grid_pos.x + 1, grid_pos.y, grid_pos, power );
    UpdateLight( grid_pos.x, grid_pos.y - 1, grid_pos, power );
    UpdateLight( grid_pos.x, grid_pos.y + 1, grid_pos, power );

    UpdateLight( grid_pos.x - 1, grid_pos.y - 1, grid_pos, power );
    UpdateLight( grid_pos.x + 1, grid_pos.y - 1, grid_pos, power );
    UpdateLight( grid_pos.x + 1, grid_pos.y + 1, grid_pos, power );
    UpdateLight( grid_pos.x - 1, grid_pos.y + 1, grid_pos, power );
}
void World::UpdateLight( Tree::Vec2i grid_pos, Tree::Vec2i origin, float source_power )
{
    const Tree::Vec2i dist = grid_pos - origin;
    int walk_dist = std::abs( dist.x ) + std::abs( dist.y );
    IncrLight( grid_pos, source_power / walk_dist );
}
void World::IncrLight( int x, int y, float power )
{
    if( IsValid( x, y ) ) {
        IncrLight( tiles[x][y], power );
    }
}
void World::IncrLight( TilePtr tile, float power )
{
    tile->SetLight( power + tile->GetLight() );
}
