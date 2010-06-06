#pragma once

#include <boost/shared_ptr.hpp>

#include "Tree/GameState.hpp"
#include "Tree/Timer.hpp"
#include "Tree/Graphics.hpp"

class Help : public Tree::GameState {
public:
    Help();
    ~Help();

    bool HandleEvent( sf::Event &e );

    void Update( float dt );
    void Draw();
private:
    sf::Sprite spr;
};

