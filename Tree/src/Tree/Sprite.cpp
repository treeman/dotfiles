#include "Sprite.hpp"
#include "Tree/Log.hpp"
#include "Tree/Butler.hpp"

#include <boost/foreach.hpp>

using Tree::SimpleSprite;
using Tree::Sprite;
using Tree::SpriteLoader;

Sprite::Sprite()
{

}

void Sprite::Draw()
{
    BOOST_FOREACH( boost::shared_ptr<SimpleSprite> s, sprites ) {
        Tree::Draw( s->spr );
    }
}

void Sprite::Render( sf::RenderTarget &target ) const
{
    BOOST_FOREACH( boost::shared_ptr<SimpleSprite> s, sprites ) {
        target.Draw( s->spr );
    }
}

SpriteLoader::SpriteLoader() { }

void SpriteLoader::Load( std::string lua_file ) throw( Error::lua_error & )
{
    L_ << "loading sprite file '" << lua_file << "'";
    LuaState L;

    if( luaL_dofile( L, lua_file.c_str() ) ) {
        const char *str = lua_tostring( L, -1 );
        lua_pop( L, -1 );
        throw( Error::lua_error( str ) );
    }

    lua_getglobal( L, "sprites" );
    for( lua_pushnil( L ); lua_next( L, -2 ); lua_pop( L, 1 ) )
    {
        if( lua_istable( L, -1 ) ) {
            boost::shared_ptr<Sprite> sprite( new Sprite() );

            std::string name = lua_tostring( L, -2 );
            if( !LoadSprite( L, sprite ) ) {

                for( lua_pushnil( L ); lua_next( L, -2 ); lua_pop( L, 1 ) )
                {
                    LoadSprite( L, sprite );
                }
            }

            if( sprite->sprites.size() ) {
                sprite_map.insert( std::make_pair( name, sprite ) );
            }
        }
    }
}

boost::shared_ptr<Sprite> SpriteLoader::Get( std::string name )
{
    SpriteMap::iterator it = sprite_map.find( name );
    if( it == sprite_map.end() ) {
        throw( Error::sprite_not_found( ( "sprite " + name + " not found" ).c_str() ) );
    }
    else { return it->second; }
}

bool SpriteLoader::LoadSprite( lua_State *L, boost::shared_ptr<Sprite> spr )
{
    if( lua_istable( L, -1 ) ) {

        std::string path = "";
        float x = 0, y = 0;
        float w = 0, h = 0;
        float x_off = 0, y_off = 0;
        float hot_x = 0;
        float hot_y = 0;
        bool smoothen = false;

        luah::get_num<float>( L, "x", x );
        luah::get_num<float>( L, "y", y );

        luah::get_num<float>( L, "x_off", x_off );
        luah::get_num<float>( L, "y_off", y_off );

        luah::get_num<float>( L, "hotspot_x", hot_x );
        luah::get_num<float>( L, "hotspot_y", hot_y );

        luah::get_bool( L, "smoothen", smoothen );

        if( luah::get_num<float>( L, "w", w ) &&
            luah::get_num<float>( L, "h", h ) &&
            luah::get_string( L, "path", path ) )
        {
            boost::shared_ptr<sf::Image> img = Tree::GetButler()->GetImage( path );

            if( img ) {
                boost::shared_ptr<SimpleSprite> simple( new SimpleSprite() );

                img->SetSmooth( smoothen );
                simple->spr.SetImage( *img );
                simple->spr.SetSubRect( sf::IntRect( x, y, w, h ) );
                simple->spr.SetCenter( hot_x, hot_y );

                simple->x_off = x_off; simple->y_off = y_off;

                spr->sprites.push_back( simple );

                return true;
            }
        }
    }
    return false;
}

