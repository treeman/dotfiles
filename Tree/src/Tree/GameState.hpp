#pragma once

#include "InputHandler.hpp"

namespace Tree
{
    class Game;

    class GameState : public InputHandler {
    public:
        virtual ~GameState() { }

        virtual void Update( float dt ) = 0;
        virtual void Draw() = 0;
    };
}

