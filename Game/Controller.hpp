#pragma once

#include "Tree/InputHandler.hpp"

class Controller : public Tree::InputHandler {
public:
    virtual ~Controller() { }

    virtual bool HandleEvent( sf::Event &e ) { return true; };
    virtual void Update( float dt ) { }
};
