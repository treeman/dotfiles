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

    girl->GetLightSource().SetLightDecline( 0.01 );
    girl->GetLightSource().SetLightSpread( 1 );
    girl->GetLightSource().SetFlicker( true );

    AddCandle();
    SwitchCandle();

    Tree::GetSettings()->Register<bool>( "debug_world", false );
    Tree::GetSettings()->Register<bool>( "debug_cam", false );
    Tree::GetSettings()->Register<bool>( "debug_candles", false );
    Tree::GetSettings()->Register<bool>( "debug_light", false );
    Tree::GetSettings()->Register<bool>( "debug_moving_object", false );
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
    const Tree::Vec2f girl_pos = girl->GetPos();
    const Tree::Vec2i girl_gpos = ConvertToGridByCenter( girl_pos );

    //update tiles, attachments and reset lights which we'll compute later
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

            boost::shared_ptr<TileObject> o = tiles[x][y]->GetAttachment();
            if( o ) {
                o->Update( dt );

                if( use_fow ) {
                    o->SetLight( 0 );
                }
                else {
                    o->SetLight( 0.9 );
                }

                if( girl_gpos == Tree::Vec2i( x, y ) ) {
                    ObjectMod mod = o->GetMod();
                    if( mod.new_candle ) {
                        AddCandle();
                    }
                    if( mod.can_remove ) {
                        tiles[x][y]->Detach();
                    }
                    if( mod.is_goal ) {
                        GoalAccomplished();
                    }
                }
            }
        }
    }

    if( girl->WantsCandleChange() ) {
        SwitchCandle();
    }

    girl->Update( dt );
    UpdateCollisions( *girl );

    //update current candle
    candles[curr_candle] = girl->GetLightSource().GetRealLightPower();

    //set light from light sources
    if( girl->GetLightSource().GetLightPower() > 0 ) {
        UpdateLight( girl_gpos,
            girl->GetLightSource().GetLightPower(),
            girl->GetLightSource().GetLightSpread() );
    }
    for( size_t x = 0; x < tiles.size(); ++x ) {
        for( size_t y = 0; y < tiles[x].size(); ++y ) {
            boost::shared_ptr<TileObject> o = tiles[x][y]->GetAttachment();
            if( o ) {
                Light light = o->GetLightSource();
                if( light.GetLightPower() > 0 ) {
                    UpdateLight( Tree::Vec2i( x, y ), light.GetLightPower(),
                        light.GetLightSpread() );

                    if( Tree::GetSettings()->GetValue<bool>( "debug_light" ) ) {
                        std::stringstream ss;
                        ss << "l: " << x << "," << y << " " << light.GetLightPower();
                        Tree::Debug( ss.str() );
                    }
                }
            }
        }
    }

    CenterCam( Tree::Vec2i( girl_pos.x + tile_size / 2, girl_pos.y + tile_size / 2 ) );

    std::stringstream ss;
    if( Tree::GetSettings()->GetValue<bool>( "debug_world" ) ) {
        const sf::Input *input = &Tree::GetInput();

        const Tree::Vec2f mpos( input->GetMouseX(), input->GetMouseY() );
        const Tree::Vec2i grid_pos = ConvertToGrid( mpos );

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
    }

    if( Tree::GetSettings()->GetValue<bool>( "debug_cam" ) ) {
        ss.str("");
        ss << "cam_pos: " << cam_pos;
        Tree::Debug( ss.str() );
    }

    if( Tree::GetSettings()->GetValue<bool>( "debug_candles" ) ) {
        for( size_t i = 0; i < candles.size(); ++i ) {
            ss.str("");
            ss << "c: " << candles[i];
            Tree::Debug( ss.str() );
        }
    }

    ss.str("");
    ss << "goals: " << achieved_goals << "/" << num_goals;
    Tree::Debug( ss.str() );
}

void World::Draw()
{
    for( size_t x = 0; x < tiles.size(); ++x ) {
        for( size_t y = 0; y < tiles[x].size(); ++y ) {
            Tree::Vec2f pos = ConvertToScreen( tiles[x][y]->GetPos() );
            tiles[x][y]->Draw( pos );
            boost::shared_ptr<TileObject> o = tiles[x][y]->GetAttachment();
            if( o ) {
                o->Draw( pos );
            }
        }
    }

    girl->Draw( ConvertToScreen( girl->GetPos() ) );
}

void World::LoadLevel( Level &lvl )
{
    LevelResources resources = lvl_loader.CreateResources( lvl );

    tiles = resources.tiles;
    girl->SetPos( resources.girl_pos );

    num_goals = lvl_loader.CalculateNumGoals( tiles );
    achieved_goals = 0;

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

    if( Tree::GetSettings()->GetValue<bool>( "debug_moving_object" ) ) {
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

void World::UpdateLight( Tree::Vec2i grid_pos, float power, int spread )
{
    IncrLight( grid_pos, power );
    Tree::Vec2i check_pos;

    for( int y = -spread; y < spread + 1; ++y ) {
        for( int x = -spread; x < spread + 1; ++x ) {
            UpdateLightTile( Tree::Vec2i( grid_pos.x + x, grid_pos.y + y ),
                grid_pos, power );
        }
    }

    IncrLight( grid_pos.x, grid_pos.y, power );

    /*if( IsValid( grid_pos ) ) {
        tiles[grid_pos.x][grid_pos.y]->SetLight( power );
    }*/
}
void World::UpdateLightTile( Tree::Vec2i grid_pos, Tree::Vec2i origin,
    float source_power )
{
    if( IsVisiblePathClear( grid_pos, origin ) ) {
        if( grid_pos == origin ) {
            IncrLight( grid_pos, source_power );
        }
        else {
            const Tree::Vec2i dist = grid_pos - origin;
            IncrLight( grid_pos, source_power / dist.MagnitudeSq() );
        }
    }
}
void World::IncrLight( int x, int y, float power )
{
    if( IsValid( x, y ) ) {
        TilePtr tile = tiles[x][y];
        //prevent overruns and flickering between black (invalid)
        tile->SetLight( math::clip<float>( power + tile->GetLight(), 0, 1 ) );
        boost::shared_ptr<TileObject> o = tile->GetAttachment();
        if( o ) {
            o->SetLight( math::clip<float>( power + o->GetLight(), 0, 1 ) );
        }
    }
}

bool World::IsVisiblePathClear( Tree::Vec2i p1, Tree::Vec2i p2 )
{
    /*const Tree::Vec2i dist = p1 - p2;
    const int x_size = std::abs( dist.x );
    const int y_size = std::abs( dist.y );

    typedef std::vector<std::vector<Tree::Vec2i> > Points;
    Points points( x_size );

    for( int x = 0; x < points.size(); ++x ) {

    }*/

    return true;
}

void World::AddCandle()
{
    candles.push_back( 0.6 );
}
void World::SwitchCandle()
{
    ++curr_candle;
    if( curr_candle >= candles.size() ) {
        curr_candle = 0;
    }

    girl->GetLightSource().SetLightPower( candles[curr_candle] );
}

void World::GoalAccomplished()
{
    ++achieved_goals;
    if( achieved_goals == num_goals ) {
        NextLevel();
    }
}

