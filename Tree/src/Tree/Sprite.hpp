#pragma once

#include <string>
#include <map>

#include <boost/shared_ptr.hpp>

#include "Lua/Lua.hpp"
#include "Tree/Errorhandling.hpp"
#include "Tree/Graphics.hpp"
#include "Tree/Vec2.hpp"

namespace Tree
{
    class SimpleSprite {
    public:
        SimpleSprite() : x_off( 0 ), y_off ( 0 )
        { }

        sf::Sprite spr;
        float x_off, y_off;
    };

    class Sprite {
    public:
        Sprite();

        Tree::Vec2f GetPos() const;
        void SetPos( float x, float y );
        void SetPos( Tree::Vec2f p );

        void Draw();
    private:
        friend class SpriteLoader;

        typedef std::vector<SimpleSprite> Sprites;
        Sprites sprites;

        Tree::Vec2f pos;
    };

    class SpriteLoader {
    public:
        SpriteLoader();

        void Load( std::string lua_file ) throw( Error::lua_error & );

        Sprite Get( std::string name );
    private:
        typedef std::map<std::string, Sprite> SpriteMap;
        SpriteMap sprite_map;

        //lua helper to load a sprite
        bool LoadSprite( lua_State *L, Sprite &spr );
    };
}

