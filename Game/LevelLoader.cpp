#include "Tree/Util.hpp"
#include "Tree/Log.hpp"
#include "Tree/Tweaks.hpp"
#include "Tree/Butler.hpp"
#include "Lua/Lua.hpp"

#include "LevelResources.hpp"
#include "LevelLoader.hpp"
#include "Tile.hpp"
#include "TileTypes.hpp"

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

            if( ch == 'o' ) {
                //insert a "nothing" tile
                //so it can check for not passable more easily in world
                TilePtr tile( new BlockTile( pos ) );
                column.push_back( tile );
            }
            else {
                TilePtr tile( new Floor( pos ) );
                column.push_back( tile );
            }

            if( ch == 'G' ) {
                resources.girl_pos = pos;
            }
        }
        resources.tiles.push_back( column );
    }
    return resources;
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

    //the last entry in the linked list
    Level *curr_lvl = 0;

    //all levels
    lua_getglobal( L, "levels" );
    for( lua_pushnil( L ); lua_next( L, -2 ); lua_pop( L, 1 ) )
    {
        if( lua_istable( L, -1 ) ) {

            //get lvl name
            std::string name = lua_tostring( L, -2 );

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

                //if this is the first level
                if( curr_lvl == 0 ) {
                    lvls = curr_lvl = lvl.get();
                }
                //move forward in the list
                else {
                    lvl->prev = curr_lvl;
                    curr_lvl->next = lvl.get();
                    curr_lvl = lvl.get();
                }

                levels.push_back( lvl );
            }
        }
    }
}

