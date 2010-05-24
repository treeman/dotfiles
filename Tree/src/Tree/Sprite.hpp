#pragma once

#include <string>
#include <map>

#include <boost/shared_ptr.hpp>

#include "Lua/Lua.hpp"
#include "Tree/Errorhandling.hpp"
#include "Tree/Graphics.hpp"

namespace Tree
{
    class SimpleSprite {
    public:
        SimpleSprite() : x_off( 0 ), y_off ( 0 )
        { }

        sf::Sprite spr;
        float x_off, y_off;
    };

    class Sprite : public sf::Drawable {
    public:
        void Draw();
    private:
        Sprite();
        friend class SpriteLoader;

        void Render( sf::RenderTarget &target ) const;

        typedef std::vector<boost::shared_ptr<SimpleSprite> > Sprites;

        Sprites sprites;
    };

    class SpriteLoader {
    public:
        SpriteLoader();

        void Load( std::string lua_file ) throw( Error::lua_error & );

        boost::shared_ptr<Sprite> Get( std::string name );
    private:
        typedef std::map<std::string, boost::shared_ptr<Sprite> > SpriteMap;
        SpriteMap sprite_map;

        //lua helper to load a sprite
        bool LoadSprite( lua_State *L, boost::shared_ptr<Sprite> spr );
    };
}

