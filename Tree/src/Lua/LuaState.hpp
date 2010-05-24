#ifndef LUASTATE_HPP_INCLUDED
#define LUASTATE_HPP_INCLUDED

#include "LuaIncl.hpp"

class LuaState {
public:
    LuaState() : L( lua_open() ) {
        luaL_openlibs( L );
    }

    ~LuaState() {
        lua_close( L );
    }
    //implicitly act as a lua_State pointer
    inline operator lua_State*() {
        return L;
    }

protected:
    lua_State *L;
};

#endif
