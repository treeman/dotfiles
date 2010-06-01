#pragma once

#include <boost/shared_ptr.hpp>
#include "Controller.hpp"
#include "Ghost.hpp"

class GhostController : public Controller {
public:
    GhostController();

    typedef boost::shared_ptr<Ghost> GhostPtr;

    void Clear();
    void Attach( GhostPtr ghost );
    void Detach( GhostPtr ghost );

    bool HandleEvent( sf::Event &e );
    void Update( float dt );
private:
    typedef std::vector<GhostPtr> Ghosts;
    Ghosts ghosts;

    void UpdateGhost( GhostPtr ghost, float dt );
};

