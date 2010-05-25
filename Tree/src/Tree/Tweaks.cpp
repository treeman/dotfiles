#include "Tweaks.hpp"
#include "Tree/Errorhandling.hpp"
#include "Lua/Lua.hpp"
#include "Tree/Log.hpp"
#include "Tree/Game.hpp"

using Tree::Tweaks;

boost::shared_ptr<Tweaks> Tree::GetTweaks()
{
    return GAME->GetTweaks();
}

Tweaks::Tweaks()
{

}

void Tweaks::Load( std::string path )
{
    LuaState L;
    if( luaL_dofile( L, path.c_str() ) ) {
        const char *str = lua_tostring( L, -1 );
        lua_pop( L, -1 );
        throw( Error::lua_error( str ) );
    }

    lua_getglobal( L, "_G" );
    for( lua_pushnil( L ); lua_next( L, -2 ); lua_pop( L, 1 ) )
    {
        if( lua_isnumber( L, -1 ) ) {
            doubles[lua_tostring( L, -2 )] = (double)lua_tonumber( L, -1 );
        }
        else if( lua_isstring( L, -1 ) ) {
            strings[lua_tostring( L, -2 )] = lua_tostring( L, -1 );
        }
    }
}

double Tweaks::GetDouble( std::string s )
{
    DoubleMap::iterator it = doubles.find( s );
    if( it != doubles.end() ) { return it->second; }
    else {
        throw( Error::setting_not_found( ("The double " + s +
            " wasn't found in magic_numbers").c_str() ) );
    }
}

std::string Tweaks::GetString( std::string s )
{
    StringMap::iterator it = strings.find( s );
    if( it != strings.end() ) { return it->second; }
    else {
        throw( Error::setting_not_found( ("The string " + s +
            " wasn't found in magic_numbers").c_str() ) );
    }
}

