#ifndef LOGO_HPP_INCLUDED
#define LOGO_HPP_INCLUDED

#include <boost/shared_ptr.hpp>

#include "Tree/GameState.hpp"
#include "Tree/Timer.hpp"
#include "Tree/Graphics.hpp"

namespace Tree {

    class Logo : public Tree::GameState {
    public:
        Logo();
        ~Logo();

        bool HandleEvent( sf::Event &e );

        void Update( float dt );
        void Draw();
    private:
        Timer t;

        sf::Sprite logo;
        int fade;
        int fade_dir;
    };

}

#endif
