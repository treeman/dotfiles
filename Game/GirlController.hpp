#pragma once

#include <boost/shared_ptr.hpp>
#include "Controller.hpp"
#include "Girl.hpp"

class GirlController : public Controller {
public:
    GirlController( boost::shared_ptr<Girl> girl );

    bool HandleEvent( sf::Event &e );
    void Update( float dt );
private:
    boost::shared_ptr<Girl> girl;
};
