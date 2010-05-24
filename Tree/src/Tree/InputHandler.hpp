#ifndef INPUTHANDLER_HPP_INCLUDED
#define INPUTHANDLER_HPP_INCLUDED

#include <list>

#include <SFML/Window.hpp>

namespace Tree {

    class InputHandler {
    public:
        InputHandler() : input_priority( 1337 ) { }
        virtual ~InputHandler() { }

        //return true if event is allowed to be passed to the next handler
        virtual bool HandleEvent( sf::Event &e ) = 0;

        const int GetPriority() const {
            return input_priority;
        }
        void SetPriority( int p ) {
            input_priority = p;
        }
    private:
        int input_priority;
    };

    class InputChain {
    public:
        InputChain();

        bool HandleEvent( sf::Event &e );

        void AddHandler( InputHandler *handler );
        void RemoveHandler( InputHandler *handler );
    private:
        typedef std::list<InputHandler*> HandlerList;
        HandlerList handler_list;
    };

}

#endif
