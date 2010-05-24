#include "Tree/InputHandler.hpp"

using Tree::InputChain;

InputChain::InputChain() { }

bool InputChain::HandleEvent( sf::Event &e )
{
    for( HandlerList::iterator it = handler_list.begin(); it != handler_list.end(); ++it ) {
        if( !(*it)->HandleEvent( e ) ) {
            return false;
        }
    }
    return true;
}

void InputChain::AddHandler( InputHandler *handler )
{
    HandlerList::iterator it;
    for( it = handler_list.begin(); it != handler_list.end(); ++it ) {
        if( (*it)->GetPriority() > handler->GetPriority() ) {
            break;
        }
    }
    handler_list.insert( it, handler );
}
void InputChain::RemoveHandler( InputHandler *handler )
{
    handler_list.remove( handler );
}

