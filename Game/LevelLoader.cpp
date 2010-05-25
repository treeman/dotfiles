#include "Tree/Util.hpp"
#include "Tree/Log.hpp"
#include "Tree/Tweaks.hpp"
#include "Lua/Lua.hpp"

#include "LevelLoader.hpp"
#include "Grid.hpp"
#include "Tile.hpp"
#include "Floor.hpp"

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

TileGrid LevelLoader::CreateTiles( Level &lvl )
{
    Grid grid;

    int tile_size = (int)Tree::GetTweaks()->GetDouble( "tile_size" );
    if( tile_size ) {
        int columns = Tree::GetWindowWidth() / tile_size;
        int rows = Tree::GetWindowHeight() / tile_size;
        grid.Set( 0, tile_size, columns, 0, tile_size, rows );
    }

    TileGrid tiles;

    for( int x = 0; x < grid.GetColumns(); ++x ) {
        Tiles column;
        for( int y = 0; y < grid.GetRows(); ++y ) {
            TilePtr tile;
            Tree::Vec2i pos = grid.ConvertToScreen( GridPos( x, y ) );
            tile.reset( new Floor( pos ) );
            column.push_back( tile );
        }
        tiles.push_back( column );
    }

    return tiles;
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

