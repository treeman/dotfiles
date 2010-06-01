#include <boost/foreach.hpp>
#include <sstream>

#include "Tree/VisualDebug.hpp"
#include "Tree/Util.hpp"
#include "GhostController.hpp"

GhostController::GhostController()
{

}

void GhostController::Clear()
{
    ghosts.clear();
}
void GhostController::Attach( GhostPtr ghost )
{
    ghosts.push_back( ghost );
}
void GhostController::Detach( GhostPtr ghost )
{
    ghosts.erase( std::find( ghosts.begin(), ghosts.end(), ghost ) );
}

bool GhostController::HandleEvent( sf::Event &e )
{
    return true;
}
void GhostController::Update( float dt )
{
    BOOST_FOREACH( GhostPtr ghost, ghosts ) {
        UpdateGhost( ghost, dt );
    }
}

void GhostController::UpdateGhost( GhostPtr ghost, float dt )
{
    if( !ghost->IsMoving() && !ghost->IsPaused() ) {
        if( ghost->WantsPause() ) {
            ghost->Stop();
            ghost->Pause();
        }
        else {
            typedef std::vector<Tree::Vec2i> Dirs;
            Dirs valid_dirs = ghost->GetValidDirections();
            Tree::Vec2i dir = *math::random( valid_dirs.begin(), valid_dirs.end() );

            if( dir == Tree::Vec2i::left ) { ghost->MoveLeft(); }
            else if( dir == Tree::Vec2i::right ) { ghost->MoveRight(); }
            else if( dir == Tree::Vec2i::down ) { ghost->MoveDown(); }
            else if( dir == Tree::Vec2i::up ) { ghost->MoveUp(); }
        }
    }
}

