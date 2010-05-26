#pragma once

#include <vector>
#include <string>

#include "Tree/Graphics.hpp"

namespace Tree
{
    //for us to draw a debug message on screen from everywhere
    //it'll be a "singleton" thanks to the Game class
    void Debug( std::string message );

    class VisualDebug {
    public:
        VisualDebug();

        void EndofLoop();

        void Push( std::string debug_message );
        void Draw();
    private:
        typedef std::vector<std::string> Strings;
        Strings strings;

        sf::String str;
    };
}

