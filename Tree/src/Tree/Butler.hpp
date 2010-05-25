#pragma once

#include <string>
#include <map>
#include <boost/shared_ptr.hpp>

#include <SFML/Graphics.hpp>

#include "Tree/Singleton.hpp"
#include "Tree/ErrorHandling.hpp"
#include "Tree/Sprite.hpp"

namespace Tree
{
    class Butler;

    //retrieve butler from the game
    boost::shared_ptr<Butler> GetButler();

    class Butler {
    public:
        Butler();
        ~Butler();

        void LoadSprites( std::string lua_file );

        boost::shared_ptr<sf::Font> GetFont( std::string path,
            unsigned int size = 30 ) throw( Error::file_not_found );

        boost::shared_ptr<sf::Image> GetImage( std::string path )
            throw( Error::file_not_found );

        Sprite GetSprite( std::string name );
    private:
        typedef std::multimap<std::string, boost::shared_ptr<sf::Font> > FontMap;
        FontMap font_map;

        typedef std::map<std::string, boost::shared_ptr<sf::Image> > ImageMap;
        ImageMap image_map;

        SpriteLoader spr_loader;
    };
}

