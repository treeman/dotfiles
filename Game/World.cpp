#include <boost/foreach.hpp>

#include "Tree/Tweaks.hpp"
#include "Tree/Util.hpp"
#include "Tree/Butler.hpp"
#include "Tree/Log.hpp"
#include "Tree/Math.hpp"
#include "Tree/VisualDebug.hpp"
#include "Tree/Settings.hpp"
#include "Tree/Game.hpp"

#include "World.hpp"
#include "Victory.hpp"

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

    curr_candle = 0;
    AddCandle( Tree::GetTweaks()->GetNum( "candle_power" ) );
    SwitchCandle();

    visual_str.SetFont( *Tree::GetButler()->GetFont( "fnt/shruti.ttf", 16 ) );
    visual_str.SetSize( 16 );
    visual_str.SetColor( sf::Color( 255, 255, 255 ) );

    matches = 0;

    Tree::GetSettings()->Register<bool>( "play_music", true );

    if( Tree::GetSettings()->GetValue<bool>( "play_music" ) ) {
        music = Tree::GetButler()->GetMusic( "sfx/souls.ogg" );
        music->Play();
        music->SetVolume( 40 );
        music->SetLoop( true );
    }

    clonk_sound = Tree::GetButler()->GetSound( "sfx/clonk.wav" );
    music_box = Tree::GetButler()->GetSound( "sfx/music_box.wav" );
    music_box.SetVolume( 20 );
    tear_sound = Tree::GetButler()->GetSound( "sfx/tear.wav" );
    katjing_sound = Tree::GetButler()->GetSound( "sfx/katjing.wav" );
    katjing_sound.SetVolume( 30 );
    fire_sound = Tree::GetButler()->GetSound( "sfx/fire.wav" );
    ghost_sound = Tree::GetButler()->GetSound( "sfx/ghost.wav" );
    skeleton_sound = Tree::GetButler()->GetSound( "sfx/skeleton.wav" );
    new_level_sound = Tree::GetButler()->GetSound( "sfx/finished.wav" );
    new_level_sound.SetVolume( 30 );

    candle = Tree::GetButler()->GetSprite( "candle" );
    teddy = Tree::GetButler()->GetSprite( "teddy" );
    key = Tree::GetButler()->GetSprite( "key" );
    match = Tree::GetButler()->GetSprite( "match" );

    const float spr_y = 0;

    candle.SetPos( 60, spr_y );
    key.SetPos( 120, spr_y );
    match.SetPos( 170, spr_y );
    teddy.SetPos( 220, spr_y );

    Tree::GetSettings()->Register<bool>( "debug_world", false );
    Tree::GetSettings()->Register<bool>( "debug_cam", false );
    Tree::GetSettings()->Register<bool>( "debug_candles", false );
    Tree::GetSettings()->Register<bool>( "debug_light", false );
    Tree::GetSettings()->Register<bool>( "debug_moving_object", false );
    Tree::GetSettings()->Register<bool>( "debug_stats", false );
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
        new_level_sound.Play();
    }
    else {
        SetFirstLevel();

        boost::shared_ptr<Tree::GameState> state( new Victory() );
        Tree::Game::Instance()->Push( state );
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
    if( achieved_goals == num_goals ) {
        NextLevel();
    }

    const Tree::Vec2f girl_pos = girl->GetPos();
    const Tree::Vec2i girl_gpos = ConvertToGridByCenter( girl_pos );

    const bool use_fow = Tree::GetSettings()->GetValue<bool>( "fow" );

    //reset the girl's light
    if( use_fow ) {
        girl->SetLight( 0 );
    }
    else {
        girl->SetLight( 0.9 );
    }

    //update tiles, attachments and reset lights which we'll compute later
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
                    if( o->CanLit() ) {
                        LightCandle();
                    }

                    ObjectMod mod = o->GetMod();
                    if( mod.new_candle ) {
                        AddCandle( mod.candle_power );
                        katjing_sound.Play();
                    }
                    if( mod.can_remove ) {
                        tiles[x][y]->Detach();
                    }
                    if( mod.is_goal ) {
                        ++achieved_goals;
                        music_box.Play();
                    }
                    if( mod.is_key ) {
                        ++keys;
                        clonk_sound.Play();
                    }
                    if( mod.is_match ) {
                        ++matches;
                        katjing_sound.Play();
                    }

                    if( o->IsDoor() ) {
                        tear_sound.Play();
                        --keys;
                        if( keys < 0 ) {
                            L_ << "omgosh keys are less than 0!";
                            keys = 0;
                        }
                    }
                    if( o->CanBlowOut() ) {
                        BlowCandle();
                        skeleton_sound.Play();
                    }
                }
            }
        }
    }

    //handle girl actions
    if( girl->WantsAction() ) {
        Light light = girl->GetLightSource();

        if( !light.IsLit() && ( light.GetRealLightPower() > 0 ) ) {
            if( matches > 0 ) {
                --matches;
                LightCandle();
            }
        }
        else {
            SwitchCandle();
        }
    }

    girl->Update( dt );
    UpdateCollisions( *girl );

    ghost_controller.Update( dt );

    BOOST_FOREACH( boost::shared_ptr<Ghost> ghost, ghosts ) {
        ghost->Update( dt );
        UpdateCollisions( *ghost );

        typedef std::vector<Tree::Vec2i> FreePaths;
        FreePaths free_paths;

        const Tree::Vec2i gpos = ConvertToGridByCenter( ghost->GetPos() );

        if( girl_gpos == gpos ) {
            BlowCandle();
            ghost_sound.Play();
        }

        if( IsWalkable( gpos.x - 1, gpos.y ) ) free_paths.push_back( Tree::Vec2i::left );
        if( IsWalkable( gpos.x + 1, gpos.y ) ) free_paths.push_back( Tree::Vec2i::right );
        if( IsWalkable( gpos.x, gpos.y - 1 ) ) free_paths.push_back( Tree::Vec2i::up );
        if( IsWalkable( gpos.x, gpos.y + 1 ) ) free_paths.push_back( Tree::Vec2i::down );

        ghost->SetValidDirections( free_paths );
    }

    //if the candle burned out update the help message
    if( candles[curr_candle] > 0
        && girl->GetLightSource().GetRealLightPower() <= 0 ) {
        help_message = Tree::GetTweaks()->GetString( "on_burn_out" );
    }

    //if we have no candles left at all (or we have one and it's unusable)
    if( candles.size() == 1 && candles[curr_candle] <= 0 ) {
        help_message = Tree::GetTweaks()->GetString( "on_no_candles" );
    }

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

    //set the girl's light
    girl->SetLight( tiles[girl_gpos.x][girl_gpos.y]->GetLight() );

    //center on girl
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

    if( Tree::GetSettings()->GetValue<bool>( "debug_stats" ) ) {
        ss.str("");
        ss << "goals: " << achieved_goals << "/" << num_goals;
        Tree::Debug( ss.str() );

        ss.str("");
        ss << "keys: " << keys;
        Tree::Debug( ss.str() );

        ss.str("");
        ss << "matches: " << matches;
        Tree::Debug( ss.str() );
    }
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
    BOOST_FOREACH( boost::shared_ptr<Ghost> ghost, ghosts ) {
        ghost->Draw( ConvertToScreen( ghost->GetPos() ) );
    }

    girl->Draw( ConvertToScreen( girl->GetPos() ) );

    const float text_top_y = 7;

    std::stringstream ss;
    ss << "Level " << curr_lvl->GetLevelNum() << ": " << lvl_message;
    visual_str.SetText( ss.str() );
    visual_str.SetPosition( 300, text_top_y );
    Tree::Draw( visual_str );

    visual_str.SetText( help_message );
    visual_str.SetPosition( Tree::GetWindowWidth() / 2 - visual_str.GetRect().GetWidth() / 2, 555 );
    Tree::Draw( visual_str );

    candle.Draw();
    key.Draw();
    match.Draw();
    teddy.Draw();

    const float off = 30;

    float candle_power = candles[curr_candle];
    if( candle_power < 0.01 ) {
        candle_power = 0;
    }
    if( candle_power < 0.1 ) {
        ss.precision(1);
    }
    else {
        ss.precision(2);
    }

    ss.str("");
    ss << candle_power;
    visual_str.SetText( ss.str() );
    visual_str.SetPosition( candle.GetPos().x + off, text_top_y );
    Tree::Draw( visual_str );

    ss.str("");
    ss << keys;
    visual_str.SetText( ss.str() );
    visual_str.SetPosition( key.GetPos().x + off, text_top_y );
    Tree::Draw( visual_str );

    ss.str("");
    ss << matches;
    visual_str.SetText( ss.str() );
    visual_str.SetPosition( match.GetPos().x + off, text_top_y );
    Tree::Draw( visual_str );

    ss.str("");
    ss << achieved_goals << "/" << num_goals;
    visual_str.SetText( ss.str() );
    visual_str.SetPosition( teddy.GetPos().x + off, text_top_y );
    Tree::Draw( visual_str );
}

void World::LoadLevel( Level &lvl )
{
    LevelResources resources = lvl_loader.CreateResources( lvl );

    tiles = resources.tiles;
    girl->SetPos( resources.girl_pos );
    girl->Reset();

    num_goals = lvl_loader.CalculateNumGoals( tiles );
    achieved_goals = 0;

    ghost_controller.Clear();
    ghosts = resources.ghosts;
    BOOST_FOREACH( boost::shared_ptr<Ghost> ghost, ghosts ) {
        ghost_controller.Attach( ghost );
    }

    keys = 0;
    matches = 0;

    candles.clear();

    curr_candle = 0;
    AddCandle( Tree::GetTweaks()->GetNum( "candle_power" ) );
    SwitchCandle();

    lvl_message = "";

    girl->GetLightSource().SetLit( true );
    girl->GetLightSource().SetLightDecline( resources.candle_decline );

    if( lvl.GetName() != "" ) lvl_message += lvl.GetName() + ": ";
    lvl_message += resources.message;

    help_message = resources.help;

    curr_lvl = &lvl;
}

bool World::IsWalkable( int x, int y )
{
    if( !IsValid( x, y ) || !tiles[x][y]->IsWalkable() ) {
        return false;
    }

    boost::shared_ptr<TileObject> o = tiles[x][y]->GetAttachment();
    if( o && o->IsDoor() ) {
        return keys > 0;
    }
    else {
        return true;
    }
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
    if( p1 == p2 ) {
        return true;
    }
    else if( p1.x == p2.x ) {
        const int y_min = std::min( p1.y, p2.y );
        const int y_max = std::max( p1.y, p2.y );

        for( int y = y_min; y <= y_max; ++y ) {
            if( !IsSeeThrough( p1.x, y ) ) return false;
        }
        return true;
    }
    else if( p1.y == p2.y ) {
        const int x_min = std::min( p1.x, p2.x );
        const int x_max = std::max( p1.x, p2.x );

        for( int x = x_min; x <= x_max; ++x ) {
            if( !IsSeeThrough( x, p1.y ) ) return false;
        }
        return true;
    }
    else {
        const int x_min = std::min( p1.x, p2.x );
        const int x_max = std::max( p1.x, p2.x );
        const int y_min = std::min( p1.y, p2.y );
        const int y_max = std::max( p1.y, p2.y );

        bool result = true;
        for( int x = x_min; x <= x_max; ++x ) {
            result = result && IsVisiblePathClear(
                Tree::Vec2i( x, y_min ), Tree::Vec2i( x, y_max )
            );
        }
        return result;
    }
}

void World::AddCandle( float power )
{
    candles.push_back( power );
}
void World::SwitchCandle()
{
    if( candles.size() > 1 && candles[curr_candle] <= 0 ) {
        candles.erase( candles.begin() + curr_candle );
    }
    else {
        ++curr_candle;
    }

    if( curr_candle >= candles.size() ) {
        curr_candle = 0;
    }

    girl->GetLightSource().SetLightPower( candles[curr_candle] );
}
void World::BlowCandle()
{
    girl->GetLightSource().SetLit( false );
    help_message = Tree::GetTweaks()->GetString( "on_blow_out" );
}
void World::LightCandle()
{
    if( !girl->GetLightSource().IsLit()
            && girl->GetLightSource().GetRealLightPower() > 0
            && fire_sound.GetStatus() != sf::Sound::Playing ) {
        fire_sound.Play();
    }
    girl->GetLightSource().SetLit( true );
    if( girl->GetLightSource().GetRealLightPower() > 0 ) {
        help_message = "";
    }
}

