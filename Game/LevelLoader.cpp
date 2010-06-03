#include "Tree/Util.hpp"
#include "Tree/Log.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Butler.hpp"
#include "Lua/Lua.hpp"

#include "LevelResources.hpp"
#include "LevelLoader.hpp"
#include "Tile.hpp"
#include "TileTypes.hpp"
#include "Objects.hpp"
#include "Ghost.hpp"

LevelLoader::LevelLoader() : lvls( 0 )
{

}

bool LevelLoader::IsThereALevel()
{
    return lvls != 0;
}
Level &LevelLoader::GetFirstLevel()
{
    return *lvls;
}

LevelResources LevelLoader::CreateResources( Level &lvl )
{
    LevelResources resources;

    resources.message = lvl.message;

    int tile_size = (int)Tree::GetTweaks()->GetNum( "tile_size" );

    //this is flipping the array, columns becomes lines
    //and lines becomes columns
    //doing it here because it will make for easier lua parsing
    //levels must be square!

    //the layout has columns
    for( size_t x = 0; x < lvl.layout[0].size(); ++x ) {
        Tiles column;

        //the columns has a string (with chars)
        for( size_t y = 0; y < lvl.layout.size(); ++y ) {
            Tree::Vec2i pos( x * tile_size, y * tile_size );

            //we make tiles according to chars! yay!
            char ch = lvl.layout[y][x];

            TilePtr tile;
            if( ch == 'o' ) {
                //insert a "nothing" tile
                //so it can check for not passable more easily in world
                tile.reset( new BlockTile( pos ) );
            }
            else {
                tile.reset( new Floor( pos ) );
            }

            if( ch == 'G' ) {
                resources.girl_pos = pos;
            }
            else if( ch == 'x' ) {
                boost::shared_ptr<Ghost> ghost( new Ghost() );
                ghost->SetPos( pos );
                resources.ghosts.push_back( ghost );
            }
            else if( ch == 'l' ) {
                boost::shared_ptr<TileObject> o( new LightObject() );
                tile->Attach( o );
            }
            else if( ch == 'C' ) {
                boost::shared_ptr<Candle> o( new Candle( 1, true ) );
                tile->Attach( o );
            }
            else if( ch == 'c' ) {
                boost::shared_ptr<Candle> o( new Candle( 1 ) );
                tile->Attach( o );
            }
            else if( ch == 'S' ) {
                boost::shared_ptr<Candle> o( new Candle( 0, true ) );
                tile->Attach( o );
            }
            else if( ch == 's' ) {
                boost::shared_ptr<Candle> o( new Candle( 0 ) );
                tile->Attach( o );
            }
            else if( ch == 'T' ) {
                boost::shared_ptr<Teddy> o( new Teddy( true ) );
                tile->Attach( o );
            }
            else if( ch == 't' ) {
                boost::shared_ptr<Teddy> o( new Teddy() );
                tile->Attach( o );
            }
            else if( ch == 'd' ) {
                boost::shared_ptr<Door> o( new Door() );
                tile->Attach( o );
            }
            else if( ch == 'D' ) {
                boost::shared_ptr<Door> o( new Door( true ) );
                tile->Attach( o );
            }
            else if( ch == 'k' ) {
                boost::shared_ptr<Key> o( new Key() );
                tile->Attach( o );
            }
            else if( ch == 'K' ) {
                boost::shared_ptr<Key> o( new Key( true ) );
                tile->Attach( o );
            }
            else if( ch == 'q' ) {
                boost::shared_ptr<Skeleton> o( new Skeleton() );
                tile->Attach( o );
            }
            else if( ch == 'Q' ) {
                boost::shared_ptr<Skeleton> o( new Skeleton( true ) );
                tile->Attach( o );
            }

            column.push_back( tile );
        }
        resources.tiles.push_back( column );
    }
    return resources;
}
int LevelLoader::CalculateNumGoals( TileGrid &grid )
{
    int n = 0;
    for( size_t x = 0; x < grid.size(); ++x ) {
        for( size_t y = 0; y < grid[x].size(); ++y ) {
            boost::shared_ptr<TileObject> o = grid[x][y]->GetAttachment();
            if( o ) {
                ObjectMod mod = o->GetMod();
                if( mod.is_goal) ++n;
            }
        }
    }
    return n;
}

void LevelLoader::LoadLevelFile( std::string file ) throw( Error::lua_error & )
{
    L_ << "loading lvl file '" << file << "'";
    LuaState L;

    if( luaL_dofile( L, file.c_str() ) ) {
        const char *str = lua_tostring( L, -1 );
        lua_pop( L, -1 );
        throw( Error::lua_error( str ) );
    }

    //all levels
    lua_getglobal( L, "levels" );
    for( lua_pushnil( L ); lua_next( L, -2 ); lua_pop( L, 1 ) )
    {
        if( lua_istable( L, -1 ) ) {

            //get lvl name
            std::string name;
            if( lua_isstring( L, -2 ) ) {
                //lua_tostring( L, -2 );
            }

            //lvl message
            std::string message;
            luah::get_string( L, "message", message );

            //lvl num (for sorting the levels)
            int lvl_num = 0;
            luah::get_num( L, "num", lvl_num );

            Level::Layout layout;

            //get lvl layout
            lua_pushstring( L, "layout" );
            lua_gettable( L, -2 );

            if( lua_istable( L, -1 ) )
            {
                for( lua_pushnil( L ); lua_next( L, -2 ); lua_pop( L, 1 ) )
                {
                    if( lua_isstring( L, -1 ) ) {
                        //get layout line
                        std::string line = lua_tostring( L, -1 );
                        layout.push_back( line );
                    }
                }
            }
            lua_pop( L, 1 );

            //if the level has at least one layout line it's valid
            if( layout.size() > 0 )
            {
                boost::shared_ptr<Level> lvl( new Level() );

                lvl->name = name;
                lvl->layout = layout;
                lvl->message = message;
                lvl->lvl_num = lvl_num;

                Levels::iterator it;
                for( it = levels.begin(); it != levels.end(); ++it ) {
                    if( lvl->lvl_num < (*it)->lvl_num ) break;
                    L_ << "iterating";
                }
                levels.insert( it, lvl );
            }
        }
    }

    if( !levels.size() ) throw( Error::lua_error( "No levels loaded!" ) );

    //create a linked list from the levels
    Level *curr_lvl = lvls = levels.front().get();

    for( Levels::iterator it = levels.begin() + 1; it != levels.end(); ++it ) {
        (*it)->prev = curr_lvl;
        curr_lvl->next = it->get();
        curr_lvl = it->get();
    }
}

