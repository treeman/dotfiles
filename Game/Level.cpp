#include "Tree/ErrorHandling.hpp"
#include "Level.hpp"

Level::Level() : next( 0 ), prev( 0 )
{

}

void Level::Reset()
{

}

bool Level::IsLast()
{
    return next == 0;
}
bool Level::IsFirst()
{
    return prev == 0;
}

Level &Level::GetNext()
{
    if( next == 0 ) throw( Error::logic_error( "next level fail" ) );
    return *next;
}
Level &Level::GetPrevious()
{
    if( prev == 0 ) throw( Error::logic_error( "previos level fail" ) );
    return *prev;
}

