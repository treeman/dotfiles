#include "Tree/Loghelper.hpp"

#include "Tree/Game.hpp"

using Tree::LogHelper;

boost::shared_ptr<LogHelper> Tree::GetLogHelper()
{
    return GAME->GetLogHelper();
}

LogHelper::LogHelper() : shall_log( false ), one_iteration_key( 0 )
{
}

bool LogHelper::ShallLog() const
{
    return shall_log;
}

void LogHelper::SetLogKey( int key )
{
    one_iteration_key = key;
}

bool LogHelper::HandleEvent( sf::Event &e )
{
    if( e.Type == sf::Event::KeyPressed &&
        e.Key.Code == one_iteration_key ) {
        shall_log = true;
    }
    return true;
}

void LogHelper::EndofLoop()
{
    shall_log = false;
}
