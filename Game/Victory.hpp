#pragma once

#include <boost/shared_ptr.hpp>

#include "Tree/GameState.hpp"
#include "Tree/Timer.hpp"
#include "Tree/Graphics.hpp"

class Victory : public Tree::GameState {
public:
    Victory();
    ~Victory();

    bool HandleEvent( sf::Event &e );

    void Update( float dt );
    void Draw();
private:
    sf::Sprite spr;
    sf::Sound victory;
};

